-module(d5mantui4_input).
-export([start/1, run/1]).

start(NotifyUI) ->
	{ok, spawn_link(?MODULE, run, [NotifyUI])}.

run(NotifyUI) ->
	case cecho:getch() of
	16#0a ->
		% Upon reading a newline (\n, ENTER) halt processing until
		% the UI returns because it may spawn an exteranl program and
		% we should only resume our input reading after it has ended.
		% This is modelled by the synchronous call rather than cast in
		% event of newline characters.
		ok = gen_server:call(NotifyUI, {getch, 16#0a}, infinity);
	OtherCharacter ->
		gen_server:cast(NotifyUI, {getch, OtherCharacter})
	end,
	run(NotifyUI).
