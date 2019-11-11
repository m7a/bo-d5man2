-module(d5manapi_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
	application:start(yamerl),
	gen_server:start_link({local, d5manapi_db}, d5manapi_db,
							get_conf(db_roots), []),
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
						d5manapi_handler_query, []}
				] ++ maps:fold(fun(K, V, Tail) ->
					[{"/" ++ atom_to_list(K) ++ "/[...]",
					cowboy_static, {dir, V}}|Tail]
				end, [], get_conf(fs))
			}])},
			middlewares => [cowboy_router, cowboy_handler]
		}
	),
	d5manapi_sup:start_link().

get_conf(Key) ->
	{ok, Val} = application:get_env(d5manapi, Key),
	Val.

stop(_State) ->
	gen_server:stop(d5manapi_db),
	ok.
