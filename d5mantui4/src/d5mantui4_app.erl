-module(d5mantui4_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	d5mantui4_sup:start_link().

stop(_State) ->
	ok.
