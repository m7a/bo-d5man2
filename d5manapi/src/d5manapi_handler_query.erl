-module(d5manapi_handler_query).
-behavior(cowboy_handler).
-include_lib("d5manapi_page.hrl").
-export([init/2, allowed_methods/2, content_types_provided/2, query_xml/2]).
% https://ninenines.eu/docs/en/cowboy/2.2/guide/rest_flowcharts/

init(Req, _State) ->
	{cowboy_rest, Req, 0}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"xml">>, []}, query_xml}
		%{{<<"text">>, <<"plain">>,      []}, ls_text_plain}
	], Req, State}.

query_xml(Req, Opts) ->
	Limit = binary_to_integer(cowboy_req:header(<<"x-masysma-limit">>, Req,
								<<"100">>)),
	{[<<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<d5man>\n">>,
	lists:map(fun(Page) ->
			[<<"\t<page>\n\t\t<meta>\n">>,
			 getkv("file", Page#page.file),
			 case Page#page.section of
				0 -> <<>>;
				Sec -> getkv("section", integer_to_binary(Sec))
			 end,
			 getkv("name", Page#page.name),
			 getkv("lang", Page#page.lang),
			 case Page#page.tags of
				[] -> <<>>;
				Tags -> getkv("tags", lists:join(" ", Tags))
			 end,
			 getkv("redirect", Page#page.redirect),
			 <<"\t\t</meta>\n\t</page>\n">>]
		end,
		gen_server:call(d5manapi_db, {query, Limit, lists:join(<<"/">>,
						cowboy_req:path_info(Req))})
	), <<"</d5man>\n">>], Req, Opts}.

getkv(K, V) ->
	case V of
	undefined -> <<>>;
	_         -> [<<"\t\t\t<kv k=\"">>, K, <<"\" v=\"">>, V, <<"\"/>\n">>]
	end.
