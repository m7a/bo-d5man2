-module(d5manapi_handler_page).
-behavior(cowboy_handler).
-include_lib("d5manapi_page.hrl").
-export([init/2, allowed_methods/2, content_types_accepted/2,
	accept_content/2]).

init(Req, _State)                  -> {cowboy_rest, Req, 0}.
allowed_methods(Req, State)        -> {[<<"POST">>], Req, State}.
content_types_accepted(Req, State) -> {[{'*', accept_content}], Req, State}.

accept_content(Req, State) ->
	{Response, ReturnCode} = d5manapi_handler_query:generate_response_xml(
			gen_server:call(d5manapi_db, {page_post_updated,
			lists:join(<<"/">>, cowboy_req:path_info(Req))})),
	{stop, cowboy_req:reply(ReturnCode, #{}, Response, Req), State}.
