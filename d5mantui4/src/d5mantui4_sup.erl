-module(d5mantui4_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
	{ok, DBRoots}       = application:get_env(d5mantui4, db_roots),
	{ok, CommandEditor} = application:get_env(d5mantui4, command_editor),
	{ok, NewPageRoot}   = application:get_env(d5mantui4, newpage_root),
	Query0 = case init:get_argument(query) of
			{ok, Value} -> Value;
			_NoQueryArg -> ""
		end,
	supervisor:start_link({local, ?SERVER}, ?MODULE,
				{DBRoots, CommandEditor, NewPageRoot, Query0}).

init({DBRoots, CommandEditor, NewPageRoot, Query0}) ->
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, [
		#{id => d5mantui4_ui, start => {gen_server, start_link,
					[{local, d5mantui4_ui}, d5mantui4_ui,
			{CommandEditor, NewPageRoot, Query0}, []]}},
		#{id => d5mantui4_db, start => {gen_server, start_link,
					[{local, d5mantui4_db}, d5mantui4_db, 
			{noredir, DBRoots, d5mantui4_ui}, []]}},
		#{id => d5mantui4_input, start => {d5mantui4_input, start,
					[d5mantui4_ui]}}
	]}}.
