-module(d5manapi_handler_query).
-behavior(cowboy_handler).
-include_lib("d5manapi_page.hrl").
-export([init/2, allowed_methods/2, content_types_provided/2, query_xml/2,
	generate_response_xml/1]).
% https://ninenines.eu/docs/en/cowboy/2.2/guide/rest_flowcharts/

init(Req, _State) ->
	{cowboy_rest, Req, 0}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"xml">>, []}, query_xml}
	], Req, State}.

query_xml(Req, Opts) ->
	QueryString = cowboy_req:parse_qs(Req),
	% Newly, we permit specifying limit thorugh either header or ?limit=
	% parameter. Since we use parameters for other purposes, too.
	Limit = case cowboy_req:header(<<"x-masysma-limit">>, Req, undef) of
		undef -> case lists:keyfind(<<"limit">>, 1, QueryString) of
			 false            -> 100;
			 {_Key, RawLimit} -> binary_to_integer(RawLimit)
			 end;
		RawLimit -> binary_to_integer(RawLimit)
		end,
	{Response, ReturnCode} = generate_response_xml(gen_server:call(
			d5manapi_db, {query, Limit, lists:join(<<"/">>,
			cowboy_req:path_info(Req)), QueryString})),
	% In theory, we could return the body right here. But there does not
	% seem to be a way to set status code 404 (not found) and also return
	% formatted output?
	{stop, cowboy_req:reply(ReturnCode, #{}, Response, Req), Opts}.

generate_response_xml(QueryResult) ->
	{InnerXML, ReturnCode} = process_query_result(QueryResult),
	{[<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<d5man>\n">>,
				InnerXML, <<"</d5man>\n">>], ReturnCode}.

process_query_result(QueryResult) ->
	case QueryResult of
	{error, Error} ->
		{[<<"\t<error>">>, quote_xml(Error), <<"</error>\n">>], 404};
	List -> {lists:map(fun(Page) ->
			[<<"\t<page>\n\t\t<meta>\n">>,
			getkv("file", Page#page.file),
			case Page#page.section of
			       0 -> <<>>;
			       Sec -> getkv("section", integer_to_binary(Sec))
			end,
			getkv("name",          Page#page.name),
			getkv("title",         Page#page.title),
			getkv("lang",          Page#page.lang),
			getkv("redirect",      Page#page.redirect),
			getkv("task-type",     Page#page.task_type),
			getkv("task-priority", Page#page.task_priority),
			case Page#page.tags of
			       [] -> <<>>;
			       Tags -> getkv("tags", lists:join(" ", Tags))
			end,
			<<"\t\t</meta>\n\t</page>\n">>]
		end, List), 200}
	end.

getkv(K, V) ->
	case V of
	undefined -> <<>>;
	_         -> [<<"\t\t\t<kv k=\"">>, K, <<"\" v=\"">>,
						quote_xml(V), <<"\"/>\n">>]
	end.

% https://stackoverflow.com/questions/3339014
quote_xml(Str) -> lists:map(fun quote_xml_char/1,
				lists:flatten(io_lib:format("~s", [Str]))).
quote_xml_char($<) -> <<"&lt;">>;
quote_xml_char($>) -> <<"&gt;">>;
quote_xml_char($&) -> <<"&amp;">>;
quote_xml_char($") -> <<"&quot;">>;
quote_xml_char(C)  -> C.
