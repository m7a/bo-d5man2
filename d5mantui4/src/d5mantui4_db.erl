-module(d5mantui4_db).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl"). % fun2ms
-include_lib("d5mantui4_page.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTIALIZATION PROCEDURES %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({URLPrefix, RootList, NotifyUI}) ->
	Context = {RootList, NotifyUI},
	% cast to myself to asynchronously trigger DB loading
	gen_server:cast(d5mantui4_db, {start_load_db, URLPrefix}),
	{ok, Context}.

handle_cast({start_load_db, URLPrefix}, {RootList, NotifyUI}) ->
	T0 = erlang:monotonic_time(millisecond),
	% id := page(section)
	ets:new(page_metadata,  [set, named_table]), % id   -> #page
	ets:new(index_names,    [bag, named_table]), % name -> idlist
	ets:new(index_tags,     [set, named_table]), % tag  -> frequency
	lists:foreach(fun(Root) ->
		case file:list_dir(Root) of
		{error, Emsg} ->
			gen_server:cast(NotifyUI, {log, error, io_lib:format(
				"Could not scan ~s: ~p", [Root, Emsg])});
		{ok, Entries} ->
			Dirs = lists:filter(fun(E) ->
				% skip x- entries because they may contain
				% incompatible README.md files...
				filelib:is_dir([Root, "/", E]) and
						not lists:prefix("x-", E)
			end, Entries),
			case lists:all(fun(Str) -> is_number(catch
					list_to_integer(Str)) end, Dirs) of
			true  -> proc_root_d5man(URLPrefix,
							NotifyUI, Root, Dirs);
			false -> proc_root_repos(URLPrefix,
							NotifyUI, Root, Dirs)
			end
		end
	end, RootList),
	gen_server:cast(NotifyUI, {db_loading_complete,
				erlang:monotonic_time(millisecond) - T0}),
	{noreply, {RootList, NotifyUI}};
handle_cast(_Cast, Context) ->
	{noreply, Context}.

proc_root_d5man(URLPrefix, NotifyUI, Root, Sections) ->
	proc_filtered_files_as_documents(URLPrefix, NotifyUI, Root, Sections,
					fun(File) ->
						lists:suffix(".yml", File) or
						lists:suffix(".md",  File) or
						lists:suffix(".hot", File)
					end).

proc_filtered_files_as_documents(URLPrefix, NotifyUI, Root, Dirs, Filter) ->
	lists:foreach(fun(Repo) ->
		Subdir = [Root, "/", Repo],
		case file:list_dir(Subdir) of
		{error, Emsg} ->
			gen_server:cast(NotifyUI, {log, error,
				io_lib:format("Could not scan subdir ~s: ~p",
				[Subdir, Emsg])});
		{ok, Repofiles} ->
			lists:foreach(fun(File) ->
				proc_document(URLPrefix, NotifyUI,
							[Subdir, "/", File])
			end, lists:filter(Filter, Repofiles))
		end
	end, Dirs).

proc_root_repos(URLPrefix, NotifyUI, Root, Repos) ->
	proc_filtered_files_as_documents(URLPrefix, NotifyUI, Root, Repos,
					fun(File) ->
						(File =:= "README.md")  or
						(File =:= "manpage.md") or
						(File =:= "TODO.hot")
					end).

% DocFile absolute path but made of nested lists
proc_document(URLPrefix, NotifyUI, DocFile) ->
	try yaml_from_pandoc_md(DocFile) of
	DocumentList ->
		ProcessDoc = fun(DocMeta) ->
			Rec = document_metadata_to_record(URLPrefix,
						#page{file=DocFile}, DocMeta),
			Id = iolist_to_binary([Rec#page.name, <<"(">>,
				integer_to_binary(Rec#page.section), <<")">>]),
			ets:insert(page_metadata, {Id, Rec}),
			ets:insert(index_names,   {Rec#page.name, Id}),
			lists:foreach(fun(Tag) -> ets:update_counter(index_tags,
					Tag, {1, 1}, 1) end, Rec#page.tags)
		end,
		% droplast: do not process document content part
		% (which is often null)
		try lists:foreach(ProcessDoc, lists:droplast(DocumentList))
		catch
		Etype:_Emsg:StackTrace ->
			gen_server:cast(NotifyUI, {log, error, io_lib:format(
				"Failed to process ~s: ~p, Stack Trace: ~p",
				[DocFile, Etype, StackTrace])})
		end
	catch
	Etype:_Emsg:StackTrace ->
		gen_server:cast(NotifyUI, {log, error, io_lib:format(
				"Failed to process ~s: ~p, Stack Trace: ~p",
				[DocFile, Etype, StackTrace])})
	end.

% https://www.rosettacode.org/wiki/Read_a_file_line_by_line#Erlang
% https://hexdocs.pm/yamerl/yamerl_constr.html
yaml_from_pandoc_md(DocFile) ->
	% encoding, utf8
	{ok, IO} = file:open(DocFile, [read, binary]),
	stream_file_to_yamerl(io:get_line(IO, ''), IO, yamerl_constr:new(
				{file, "<stdin>"}, [{detailed_constr, false}])).

stream_file_to_yamerl(eof, IO, Stream) ->
	file:close(IO),
	yamerl_constr:last_chunk(Stream, <<>>);
stream_file_to_yamerl(Line, IO, Stream) ->
	case Line of
	<<"">> ->
		% process normally
		{continue, StreamNew} = yamerl_constr:next_chunk(Stream, Line),
		stream_file_to_yamerl(io:get_line(IO, ''), IO, StreamNew);
	_Other ->
		H = binary:at(Line, 0),
		% Current heuristics to terminate processing is the first line
		% encountered beginning with an uppercase letter or number. Note
		% that this heuristics is quite incomplete and might need future
		% revision.
		if
		((H >= $A) and (H =< $Z)) or ((H >= $0) and (H =< $9)) ->
			% terminate here
			stream_file_to_yamerl(eof, IO, Stream);
		true ->
			% process normally
			{continue, StreamNew} = yamerl_constr:next_chunk(Stream,
									Line),
			stream_file_to_yamerl(io:get_line(IO, ''), IO,
								StreamNew)
		end
	end.

document_metadata_to_record(URLPrefix, InRecord, DocumentMetadata) ->
	lists:foldl(fun(Entry, R) ->
		case Entry of
		{"section",        Sec}   -> R#page{section=Sec};
		{"x-masysma-name", Name}  -> R#page{name=list_to_binary(Name)};
		{"title",          Title} -> R#page{title=Title};
		{"keywords", TagList} ->
			R#page{tags=lists:map(fun list_to_binary/1, TagList)};
		{"x-masysma-redirect", Red} ->
			case URLPrefix of
			noredir    -> R#page{redirect=Red};
			noupdate   -> R;
			_URLPrefix -> R#page{redirect=[URLPrefix,
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
		{"x-masysma-task-type",     Type}     -> R#page{task_type=Type};
		{"x-masysma-task-priority", Priority} -> R#page{
							task_priority=Priority};
		{_Field,_Val} -> R % silently ignore other fields
		end
	end, InRecord, DocumentMetadata).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE QUERYING %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Limit := Max number of results to return
% Query := Free-text string entry by user
% GF    := General Filter        (all | pages    | tasks)
% TFT   := Task Filter Top Level (all | toplevel | subtask)
% TFD   := Task Filter Delayed   (all | current  | delayed)
handle_call({query, Limit, Query, GF, TFT, TFD}, _From, Context) ->
	{FilterSection, QueryRem} = case string:split(erlang:list_to_binary(
					[string:trim(Query)]), " ", all) of
		[<<>>] -> {any, []};
		[QueryBegin|QueryTail] ->
			case is_number(catch binary_to_integer(QueryBegin)) of
			true  -> {binary_to_integer(QueryBegin), QueryTail};
			false -> {any, [QueryBegin|QueryTail]}
			end
		end,
	MatchSpec = ets:fun2ms(fun({ID, P}) when
	% -- matching --
	((FilterSection =:= any orelse P#page.section =:= FilterSection)
	andalso (GF =:= all
		orelse (GF =:= pages andalso P#page.task_type =:= undefined
				andalso P#page.task_priority =:= undefined)
		orelse (GF =:= tasks andalso P#page.task_type =/= undefined
				andalso P#page.task_priority =/= undefined
			andalso (
				TFT =:= all
				orelse (TFT =:= toplevel andalso
						P#page.task_type =/= "subtask")
				orelse (TFT =:= subtask andalso
						P#page.task_type =:= "subtask")
			) andalso (
				TFD =:= all
				orelse (TFD =:= current andalso
					P#page.task_priority =/= "delayed")
				orelse (TFD =:= delayed andalso
					P#page.task_priority =:= "delayed")
			))
	)) -> P end),
	% -- end matching --
	DirectMatch = case QueryRem of
		[H|[]] ->
			% ensure that e.g. section filter is applied in case
			% of index names ambinguity. task filters are most
			% likely to only be relevant in pathological cases here.
			ets:match_spec_run(lists:flatmap(fun({_K, ID}) -> 
						ets:lookup(page_metadata, ID)
					end, ets:lookup(index_names,
						iolist_to_binary(H))),
				ets:match_spec_compile(MatchSpec));
		_Others -> []
		end,
	{reply, case DirectMatch of
		% no direct match
		[] -> sort_results(query_scan(Limit, QueryRem,
				ets:select(page_metadata, MatchSpec, Limit)));
		% direct match query complete
		_Match -> DirectMatch
	end, Context};
handle_call({query_tags, Limit, Prefix}, _From, Context) ->
	MatchLen = byte_size(Prefix),
	MatchSpec = ets:fun2ms(fun({Tag, Count}) when
				binary_part(Tag, MatchLen) =:= Prefix ->
			{Tag, Count}
		end),
	{reply, lists:map(fun({Tag, Count}) ->
				#page{section=min(99, Count), name=Tag} end,
		lists:sort(fun({_TagA, CountA}, {_TagB, CountB}) ->
				CountA > CountB end,
		case ets:select(index_tags, MatchSpec, Limit) of
			'$end_of_table' -> []; List -> List
		end)), Context};
% PageID format is page_name(section). this is a key into page_metadata table
% which will point us to the file to consult. Returns io list.
handle_call({page_post_updated, PageID}, _From, Context) ->
	Key = iolist_to_binary(PageID),
	{reply, case ets:lookup(page_metadata, Key) of
	% TODO SHOULD BE ABLE TO CREATE A PAGE THAT IS NOT KNOWN TO THE DB YET!
	%      SCAN FOR THE PAGE NAME IN THE KNOWN ROOTS?
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

query_scan(_Limit, _QConsider, '$end_of_table') -> [];
query_scan(Limit, QConsider, {Entries, Continuation}) ->
	QueryResults = lists:filter(fun(PageRec) ->
				page_matches_all_q(QConsider,
					[PageRec#page.name|PageRec#page.tags])
			end, Entries),
	NRSLT = length(QueryResults),
	if
	NRSLT =:= 0   -> query_scan(Limit, QConsider, ets:select(Continuation));
	NRSLT < Limit -> QueryResults ++ query_scan(Limit - NRSLT, QConsider,
						ets:select(Continuation));
	true          -> lists:sublist(QueryResults, Limit) % NRSLT >= Limit
	end.

page_matches_all_q([], _CheckList)   -> true;
page_matches_all_q([H|T], CheckList) -> page_matches_qsingle(H, CheckList)
				andalso page_matches_all_q(T, CheckList).

page_matches_qsingle(_QSingle, [])   -> false;
page_matches_qsingle(QSingle, [H|T]) -> binary:longest_common_prefix([QSingle,
					H]) =:= byte_size(QSingle) orelse
					page_matches_qsingle(QSingle, T).

sort_results(ResultList) ->
	lists:sort(fun(A, B) ->
		TypeA = type_ordering(B#page.task_type),
		TypeB = type_ordering(B#page.task_type),
		PrioA = priority_ordering(A#page.task_priority),
		PrioB = priority_ordering(B#page.task_priority),

		if
		A#page.section < B#page.section -> true;
		A#page.section > B#page.section -> false;
		% A#page.section =:= B#page.section
		TypeA < TypeB -> true;
		TypeA > TypeB -> false;
		% A#page.task_type =:= B#page.task_type
		PrioA < PrioB -> true;
		PrioA > PrioB -> false;
		% A#page.task_priority =:= B#page.task_priority
		true -> A#page.name < B#page.name
		end
	end, ResultList).

priority_ordering("red")    -> 10;
priority_ordering("yellow") -> 20;
priority_ordering("green")  -> 30;
priority_ordering("purple") -> 40;
priority_ordering("black")  -> 50;
priority_ordering("white")  -> 60;
priority_ordering(_Other)   -> 99.

type_ordering(undefined)  -> 1000;
type_ordering("long")     -> 2000;
type_ordering("periodic") -> 3000;
type_ordering("short")    -> 4000;
type_ordering(_Other)     -> 5000.

handle_info(_Message, Context) -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok, Context}.
