-module(d5manapi_db).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-behaviour(gen_server).
-include_lib("d5manapi_page.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTIALIZATION PROCEDURES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% http://erlang.org/doc/man/file.html list_dir 
init([URLPrefix, RootList]) ->
	% id := page(section)
	io:fwrite("[INFO ] Reading DB...~n"),
	T0 = erlang:monotonic_time(millisecond),
	ets:new(page_metadata,  [set, named_table]), % id -> #page
	ets:new(index_names,    [bag, named_table]), % name -> idlist
	lists:foreach(fun(Root) ->
		case file:list_dir(Root) of
		{error, Emsg} ->
			io:fwrite(["[ERROR] Could not scan ", Root, ": ",
						atom_to_list(Emsg), "~n"]);
		{ok, Entries} ->
			Dirs = lists:filter(fun(E) ->
				% skip x- entries because they may contain
				% incompatible README.md files...
				filelib:is_dir([Root, "/", E]) and
						not lists:prefix("x-", E)
			end, Entries),
			case lists:all(fun(Str) -> is_number(catch
					list_to_integer(Str)) end, Dirs) of
			true  -> proc_root_d5man(URLPrefix, Root, Dirs);
			false -> proc_root_repos(URLPrefix, Root, Dirs)
			end
		end
	end, RootList),
	io:fwrite(["[INFO ] DB load completed after ", integer_to_list(
			erlang:monotonic_time(millisecond) - T0), "ms"]),
	io:fwrite("~n"),
	{ok, RootList}.

proc_root_d5man(URLPrefix, Root, Sections) ->
	proc_filtered_files_as_documents(URLPrefix, Root, Sections, fun(File) ->
		lists:suffix(".yml", File) or lists:suffix(".md", File) or
		lists:suffix(".hot", File)
	end).

proc_filtered_files_as_documents(URLPrefix, Root, Dirs, Filter) ->
	lists:foreach(fun(Repo) ->
		Subdir = [Root, "/", Repo],
		case file:list_dir(Subdir) of
		{error, Emsg} ->
			io:fwrite(["[ERROR] Could not scan subdir ", Subdir,
				": ", atom_to_list(Emsg), "~n"]);
		{ok, Repofiles} ->
			lists:foreach(fun(File) ->
				proc_document(URLPrefix, [Subdir, "/", File])
			end, lists:filter(Filter, Repofiles))
		end
	end, Dirs).

proc_root_repos(URLPrefix, Root, Repos) ->
	proc_filtered_files_as_documents(URLPrefix, Root, Repos, fun(File) ->
		(File =:= "README.md") or (File =:= "manpage.md") or
		(File =:= "TODO.hot")
	end).

% DocFile absolute path but made of nested lists
proc_document(URLPrefix, DocFile) ->
	try yaml_from_pandoc_md(DocFile) of
	DocumentList ->
		ProcessDoc = fun(DocMeta) ->
			Rec = document_metadata_to_record(URLPrefix,
						#page{file=DocFile}, DocMeta),
			Id = iolist_to_binary([Rec#page.name, <<"(">>,
				integer_to_binary(Rec#page.section), <<")">>]),
			ets:insert(page_metadata, {Id, Rec}),
			ets:insert(index_names,   {Rec#page.name, Id})
		end,
		% droplast: do not process document content part
		% (which is often null)
		try lists:foreach(ProcessDoc, lists:droplast(DocumentList))
		catch
		Etype:_Emsg:StackTrace ->
			io:fwrite(["[ERROR] Failed to process ", DocFile, ": ",
						atom_to_list(Etype), "\n"]),
			erlang:display(StackTrace)
		end
	catch
	Etype:_Emsg:StackTrace ->
		io:fwrite(["[ERROR] Failed to process ", DocFile, ": ",
						atom_to_list(Etype), "\n"]),
		erlang:display(StackTrace)
	end.

% https://www.rosettacode.org/wiki/Read_a_file_line_by_line#Erlang
% https://hexdocs.pm/yamerl/yamerl_constr.html
yaml_from_pandoc_md(DocFile) ->
	{ok, IO} = file:open(DocFile, [read]),
	stream_file_to_yamerl(io:get_line(IO, ''), IO, yamerl_constr:new(
				{file, "<stdin>"}, [{detailed_constr, false}])).

stream_file_to_yamerl(eof, IO, Stream) ->
	file:close(IO),
	yamerl_constr:last_chunk(Stream, <<>>);
stream_file_to_yamerl(Line, IO, Stream) ->
	case Line of
	"" ->
		% process normally
		{continue, StreamNew} = yamerl_constr:next_chunk(Stream, Line),
		stream_file_to_yamerl(io:get_line(IO, ''), IO, StreamNew);
	[H|_T] ->
		% Current heuristics to terminate processing is the first line
		% encountered beginning with an uppercase letter or number. Note
		% that this heuristics is quite incomplete and might need future
		% revision.
		if ((H >= $A) and (H =< $Z)) or ((H >= $0) and (H =< $9)) ->
			% terminate here
			stream_file_to_yamerl(eof, IO, Stream);
		true ->
			% process normally
			{continue, StreamNew} = yamerl_constr:next_chunk(Stream,
									Line),
			stream_file_to_yamerl(io:get_line(IO, ''), IO,
								StreamNew)
		end
	% other cases should not exist...
	end.

document_metadata_to_record(URLPrefix, InRecord, DocumentMetadata) ->
	lists:foldl(fun(Entry, R) ->
		case Entry of
		{"section",Sec} ->
			R#page{section=Sec};
		{"x-masysma-name",Name} ->
			R#page{name=list_to_binary(Name)};
		{"title",Title} ->
			R#page{title=Title};
		{"keywords",TagList} ->
			R#page{tags=lists:map(fun list_to_binary/1, TagList)};
		{"x-masysma-redirect",Red} ->
			case URLPrefix of
			noredir -> R#page{redirect=Red};
			noupdate -> R;
			_URlPrefix -> R#page{redirect=[URLPrefix,
					% get directory name. Do not use
					% section for this because it might not
					% be available yet!
					filename:basename(
						filename:dirname(R#page.file)),
					"/",
					filename:basename(R#page.file,
					filename:extension(R#page.file)),
					"_att/", Red]}
			end;
		{"x-masysma-task-type",Type} ->
			R#page{task_type=Type};
		{"x-masysma-task-priority",Priority} ->
			R#page{task_priority=Priority};
		{_Field,_Val} ->
			R % silently ignore other fields
		end
	end, InRecord, DocumentMetadata).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE QUERYING %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({query, Limit, Query, QSVals}, _From, Context) ->
	case get_task_filter_for_qsvals(QSVals) of
	{error, Error} -> {reply, {error, Error}, Context};
	TaskFilter ->
		case string:split(erlang:list_to_binary([string:trim(Query)]),
								" ", all) of
		[<<>>] -> {reply, sort_results(query_full_table_scan([], Limit,
							TaskFilter)), Context};
		QueryParts ->
			[QueryBegin|_QueryTail] = QueryParts,
			case binary:first(QueryBegin) of
			$:     -> respond_to_google_query(Query, Context);
			_Other -> respond_to_normal_query(QueryParts,
						TaskFilter, Limit, Context)
			end
		end
	end;
% PageID format is page_name(section). this is a key into page_metadata table
% which will point us to the file to consult. Returns io list.
handle_call({page_post_updated, PageID}, _From, Context) ->
	Key = iolist_to_binary(PageID),
	{reply, case ets:lookup(page_metadata, Key) of
	[] -> {error, ["No page found to match provided ID. Nothing updated."]};
	[{_Val, PageMetadata}]->
		% some similarities with proc_document but not exactly the same
		DocFile = PageMetadata#page.file,
		try yaml_from_pandoc_md(DocFile) of
		DocumentList ->
			ProcessDoc = fun(DocMeta) ->
				NewMetadata = document_metadata_to_record(
					noupdate,
					#page{file=PageMetadata#page.file,
					redirect=PageMetadata#page.redirect},
					DocMeta
				),
				% replaces existent item
				ets:insert(page_metadata, {Key, NewMetadata}),
				NewMetadata
			end,
			try lists:map(ProcessDoc,
						lists:droplast(DocumentList))
			catch
			_Etype:Emsg:_StackTrace ->
				{error, ["Failed to process ", DocFile, ": ",
									Emsg]}
			end
		catch
		_Etype:_Emsg:_StackTrace ->
			{error, ["Failed to process ", DocFile]}
		end
	end, Context};
handle_call(_Call, _From, Context) -> {reply, 0, Context}.

get_task_filter_for_qsvals(QSVals) ->
	case lists:keyfind(<<"task">>, 1, QSVals) of
	{_Key, <<"board">>} -> fun(Record) ->
			is_task(Record) andalso
			not(is_task_delayed(Record)) andalso
			not(is_task_subtask(Record))
		end;
	{_Key, <<"subtask">>} -> fun(Record) ->
			is_task(Record) andalso is_task_subtask(Record)
		end;
	{_Key, <<"delayed">>} -> fun(Record) ->
			is_task(Record) andalso is_task_delayed(Record)
		end;
	{_Key, <<"no">>} -> fun(Record) -> not(is_task(Record)) end;
	{_Key, _Other} -> {error,
		<<"Invalid task filter. Try: board | subtask | delayed | no">>};
	% identity filter / empty filter
	false -> fun(_Record) -> true end
	end.

is_task(Record) -> Record#page.task_type =/= undefined andalso
					Record#page.task_priority =/= undefined.
is_task_subtask(Record) -> Record#page.task_type =:= "subtask".
is_task_delayed(Record) -> Record#page.task_priority =:= "delayed".

% Catches all : queries but returns empty for everything but the two old
%	:google:<term>
%	:google:img:<term>
% commands.
respond_to_google_query(Query, Context) ->
	[_Ignore|[Cmd|Args]] = string:split(Query, ":", all),
	case Cmd of
	<<"google">> ->
		% let's build a Google search string.
		QPart = case Args of
			% build image search string
			[<<"img">>|Q] -> [cow_uri:urlencode(iolist_to_binary(Q)
						), <<"&site=imghp&tbm=isch">>];
			% build general search string
			Q2 -> [cow_uri:urlencode(iolist_to_binary(Q2))]
		end,
		URL = [<<"https://www.google.com/search?q=">>|QPart],
		{reply, [#page{name=[<<"ial/google">>],section=21,
			tags=[<<"ial">>,<<"google">>],redirect=URL}], Context};
	_Other ->
		{reply, [], Context}
	end.

respond_to_normal_query(QueryParts=[QueryBegin|QueryTail], TaskFilter, Limit,
								Context) ->
	{ResultFilter, QConsider} = case is_number(catch binary_to_integer(
								QueryBegin)) of
		true ->  CmpNum = binary_to_integer(QueryBegin),
			 {fun(Record) -> TaskFilter(Record) andalso
				Record#page.section =:= CmpNum end, QueryTail};
		false -> {TaskFilter, QueryParts}
	end,
	{reply, sort_results(query_by_parts(Limit, ResultFilter, QConsider)),
								Context}.

sort_results(ResultList) ->
	lists:sort(fun(A, B) ->
		PrioA = priority_ordering(A#page.task_priority),
		PrioB = priority_ordering(B#page.task_priority),

		if
		A#page.section < B#page.section -> true;
		A#page.section > B#page.section -> false;
		% TODO z might sort after type periodic/long/short, too to allow red to be used in both places w/o re-ordering
		% A#page.section =:= B#page.section
		PrioA < PrioB -> true;
		PrioA > PrioB -> false;
		% A#page.section =:= B#page.section and PrioA =:= PrioB
		true -> A#page.name < B#page.name
		end
	end, ResultList).

priority_ordering("yellow") -> 10;
priority_ordering("red")    -> 20;
priority_ordering("green")  -> 30;
priority_ordering("purple") -> 40;
priority_ordering("black")  -> 50;
priority_ordering("white")  -> 60;
priority_ordering(_Other)   -> 99.

query_by_parts(Limit, ResultFilter, QConsider) ->
	case QConsider of
	[H|[]] -> case ets:lookup(index_names, iolist_to_binary(H)) of
			[] -> query_full_table_scan(QConsider, Limit,
								ResultFilter);
			Matched -> lists:filter(ResultFilter, lists:map(
				fun({_K, V}) ->
					[{_Val, PageMeta}] = ets:lookup(
							page_metadata, V),
					PageMeta
				end,
				Matched))
		  end;
	_QConsider -> query_full_table_scan(QConsider, Limit, ResultFilter)
	end.

query_full_table_scan(QConsider, Limit, ResultFilter) ->
	query_full_table_scan_sub(QConsider, Limit, 0, ResultFilter,
						ets:first(page_metadata), []).

query_full_table_scan_sub(_QConsider, _Limit, _Have, _ResultFilter,
				'$end_of_table', Accumulator) -> Accumulator;
query_full_table_scan_sub(QConsider, Limit, Have, ResultFilter, PageID,
								Accumulator) ->
	[{_PageID, PageRec}] = ets:lookup(page_metadata, PageID),
	{HaveNew, AccNew} = case ResultFilter(PageRec) andalso
					page_matches(QConsider, PageRec) of
				true  -> {Have + 1, [PageRec|Accumulator]};
				false -> {Have,     Accumulator}
			    end,
	case ((Limit =:= 0) or (HaveNew < Limit)) of
	true  -> query_full_table_scan_sub(QConsider, Limit, HaveNew,
			ResultFilter, ets:next(page_metadata, PageID), AccNew);
	false -> AccNew
	end.

page_matches(QConsider, PageRec) ->
	page_matches_all_q(QConsider, [PageRec#page.name|PageRec#page.tags]).

page_matches_all_q([], _CheckList)   -> true;
page_matches_all_q([H|T], CheckList) -> page_matches_qsingle(H, CheckList)
				andalso page_matches_all_q(T, CheckList).

page_matches_qsingle(_QSingle, [])   -> false;
page_matches_qsingle(QSingle, [H|T]) -> binary:longest_common_prefix([QSingle,
					H]) =:= byte_size(QSingle) orelse
					page_matches_qsingle(QSingle, T).

handle_cast(_Cast, Context) -> {noreply, Context}.
handle_info(_Message, Context) -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
