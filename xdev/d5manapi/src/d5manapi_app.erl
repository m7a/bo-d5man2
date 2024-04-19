-module(d5manapi_app).
-behaviour(application).
-export([start/2, stop/1, mimetype/1]).

start(_Type, _Args) ->
	%{ok, UserHome} = init:get_argument(home),
	%UserConf = [UserHome, ".mdvl/d5man/d5manapi.erl"]
	application:start(yamerl),
	gen_server:start_link(
		{local, d5manapi_db}, d5manapi_db,
		[application:get_env(d5manapi, redirect_url_prefix, noredir),
							get_conf(db_roots)],
		[]
	),
	{ok, _} = cowboy:start_clear(
		my_http_listener,
		[
			{port, get_conf(port)},
			{ip,   get_conf(ip)}
		],
		#{
			env => #{ dispatch => cowboy_router:compile([{'_',
				[
					{"/query/[...]",
						d5manapi_handler_query, []},
					{"/page/[...]",
						d5manapi_handler_page, []}
				] ++ maps:fold(fun(K, V, Tail) ->
					[{"/" ++ atom_to_list(K) ++ "/[...]",
					cowboy_static,
					{dir, V,
					[{mimetypes, ?MODULE, mimetype}]}}|
					Tail]
				end, [], get_conf(fs))
			}])},
			middlewares => [cowboy_router, cowboy_handler]
		}
	),
	d5manapi_sup:start_link().

mimetype(Path) ->
	case filename:extension(Path) of
	<<".svg">>   -> {<<"image">>, <<"svg+xml">>, []};
	<<".css">>   -> {<<"text">>, <<"css">>, []};
	<<".gif">>   -> {<<"image">>, <<"gif">>, []};
	<<".png">>   -> {<<"image">>, <<"png">>, []};
	<<".js">>    -> {<<"application">>, <<"javascript">>, []};
	<<".xml">>   -> {<<"application">>, <<"xml">>, []};
	<<".xhtml">> -> {<<"application">>, <<"xhtml+xml">>, []};

	B when (B =:= <<".txt">>) or (B =:= <<".csv">>) or (B =:= <<".md">>) ->
			{<<"text">>, <<"plain">>, []};

	B when (B =:= <<".jpg">>) or (B =:= <<".jpeg">>) ->
			{<<"image">>, <<"jpeg">>, []};

	B when (B =:= <<".html">>) or (B =:= <<".htm">>) ->
			{<<"text">>, <<"html">>, []};

	_            -> {<<"application">>, <<"octet-stream">>, []}
	end.

get_conf(Key) ->
	{ok, Val} = application:get_env(d5manapi, Key),
	Val.

stop(_State) ->
	gen_server:stop(d5manapi_db),
	ok.
