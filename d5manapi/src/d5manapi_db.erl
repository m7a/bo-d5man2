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
	ets:new(page_metadata,  [set, named_table]), % id -> #page
	ets:new(index_names,    [bag, named_table]), % name -> idlist
	lists:foreach(fun(Root) ->
		case file:list_dir(Root) of
		{error, Emsg} ->
			io:fwrite(["[ERROR] Could not scan ", Root, ": ",
						atom_to_list(Emsg), "~n"]);
		{ok, Entries} ->
			Dirs = lists:filter(fun(E) ->
				filelib:is_dir([Root, "/", E])
			end, Entries),
			case lists:all(fun(Str) ->is_number(catch
					list_to_integer(Str)) end, Dirs) of
			true  -> proc_root_d5man(URLPrefix, Root, Dirs);
			false -> proc_root_repos(URLPrefix, Root, Dirs)
			end
		end
	end, RootList),
	% TODO z add statistics on how long it took to load the DB
	io:fwrite("[INFO ] DB load complete~n"),
	{ok, RootList}.

proc_root_d5man(URLPrefix, Root, Sections) ->
	proc_filtered_files_as_documents(URLPrefix, Root, Sections, fun(File) ->
		lists:suffix(".yml", File) or lists:suffix(".md", File)
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
		(File =:= "README.md") or (File =:= "manpage.md")
	end).

% DocFile absolute path but made of nested lists
proc_document(URLPrefix, DocFile) ->
	try yaml_from_pandoc_md(DocFile) of
		DocumentList ->
			lists:foreach(fun(DocMeta) ->
				% for debugging:
				%erlang:display(DocMeta),
				Rec = document_metadata_to_record(URLPrefix,
						#page{file=DocFile}, DocMeta),
				Id = iolist_to_binary([Rec#page.name,
					<<"(">>,
					integer_to_binary(Rec#page.section),
					<<")">>]),
				ets:insert(page_metadata, {Id, Rec}),
				ets:insert(index_names, {Rec#page.name, Id})
			end,
			% droplast: do not process document content part
			% (which is often null)
			lists:droplast(DocumentList))
	catch
		Etype:Emsg:StackTrace ->
			io:fwrite(["[ERROR] Failed to process ", DocFile,
						": ", atom_to_list(Etype), ":",
						atom_to_list(Emsg), "\n"]),
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
		% encountered beginning with an uppercase letter. Note that
		% this heuristics is quite incomplete and might need future
		% revision.
		if (H >= $A) and (H =< $Z) ->
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
		{"keywords",TagList} ->
			R#page{tags=lists:map(fun list_to_binary/1, TagList)};
		{"x-masysma-redirect",Red} ->
			R#page{redirect=case URLPrefix of
				noredir ->
					Red;
				_URLPrefix ->
					[URLPrefix,
					% get directory name. Do not use
					% section for this because it might not
					% be available yet!
					filename:basename(
						filename:dirname(R#page.file)),
					"/",
					filename:basename(R#page.file,
					filename:extension(R#page.file)),
					"_att/", Red]
			end};
		{_Field,_Val} ->
			R % silently ignore other fields
		end
	end, InRecord, DocumentMetadata).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DATABASE QUERYING %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({query, Limit, Query}, _From, Context) ->
	EmptyFilter = fun(_Record) -> true end,
	case lists:flatten(string:split(string:trim(Query), " ", all)) of
	[] ->   {reply, query_full_table_scan([], Limit, EmptyFilter), Context};
	QueryParts ->
		[QueryBegin|_QueryTail] = QueryParts,
		case binary:first(QueryBegin) of
		$:     -> respond_to_google_query(Query, Context);
		_Other -> respond_to_normal_query(QueryParts, EmptyFilter,
								Limit, Context)
		end
	end;
handle_call(_Call, _From, Context) ->
	{reply, 0, Context}.

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

respond_to_normal_query(QueryParts=[QueryBegin|QueryTail], EmptyFilter, Limit,
								Context) ->
	{ResultFilter, QConsider} = case is_number(catch binary_to_integer(
								QueryBegin)) of
		true ->  CmpNum = binary_to_integer(QueryBegin),
			 {fun(Record)  -> Record#page.section =:= CmpNum end,
								QueryTail};
		false -> {EmptyFilter, QueryParts}
	end,
	{reply, case QConsider of
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
		_QConsider -> query_full_table_scan(QConsider, Limit,
								ResultFilter)
	end, Context}.

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
