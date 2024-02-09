-module(d5mantui4_ui).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-behaviour(gen_server).

-include_lib("cecho/include/cecho.hrl").
-include_lib("d5mantui4_page.hrl").

-define(CPAIR_HEADING,     1).
-define(CPAIR_INPUT_BRACK, 2).
-define(CPAIR_INPUT_DATA,  3).
-define(CPAIR_CTRLKEY,     4).
-define(CPAIR_CTRLDESCR,   5).
-define(CPAIR_OUT_NORMAL,  6).
-define(CPAIR_OUT_HIGH,    7).
-define(CPAIR_TSK_RED,     8).
-define(CPAIR_TSK_GREEN,   9).
-define(CPAIR_TSK_BLACK,   10).
-define(CPAIR_TSK_WHITE,   11).
-define(CPAIR_TSK_YELLOW,  12).
-define(CPAIR_TSK_PURPLE,  13).
-define(CPAIR_TSK_DELAYED, 14).

-define(ceKEY_BACKSPACE, 263).  % TODO x PR THIS!
%-define(ceKEY_ENTER,     343).

% TODO x DEV Old D5Man TUI
% /data/main/118_man_d5i/30_man_program_etc/program/mdvl-d5man-1.0.0/d5manui/view.c
% struct d5manui_view
-record(view, {db_status, height, width, command_editor, new_page_root,
		flt_general, flt_toplevel, flt_delayed,
		main_query, main_query_subpos, main_query_max,
		wnd_title, wnd_input, wnd_subtitle, wnd_output, wnd_descr,
		main_cresult, main_cur_idx, main_cur_height}).

init({CommandEditor, NewPageRoot}) ->
	cecho:start_color(),
	init_color_pairs(),
	cecho:cbreak(),
	cecho:noecho(),
	% We used to set this for WndInput, but this requires mvwgetch() to be
	% used which is not exposed by cecho. Hence read from and configure
	% STDSCR.
	cecho:keypad(?ceSTDSCR, true),
	% Unfortunately, it does not really seem possible to handle SIGWINCH in
	% Erlang. See `erl_signal_server`
	{Height, Width}      = cecho:getmaxyx(),
	{Y1, WndTitle}       = window_create_default(Width, 2, 0),
	{Y2, WndInput}       = window_create_input(Width, Y1),
	{Y3, WndSubtitle}    = window_create_default(Width, 2, Y2 + 1),
	{Y4, WndOutput}      = window_create_default(Width, Height - 7, Y3),
	{_Y, WndDescription} = window_create_description(Width, Y4),
	display_title(Width, WndTitle,
			"Ma_Sys.ma D5Man Terminal User Interface 4.0.0"),
	display_title(Width, WndSubtitle, "Loading..."),
	{ok, #view{
		db_status         = loading,
		command_editor    = CommandEditor, % TODO MAKE USE OF
		new_page_root     = NewPageRoot,   % TODO MAKE USE OF
		height            = Height,
		width             = Width,
		flt_general       = all,
		flt_toplevel      = all,
		flt_delayed       = all,
		main_query        = "",
		main_query_max    = Width - 4,
		main_query_subpos = 0,
		wnd_title         = WndTitle,
		wnd_input         = WndInput,
		wnd_subtitle      = WndSubtitle,
		wnd_output        = WndOutput,
		wnd_descr         = WndDescription,
		main_cresult      = [],
		main_cur_idx      = 0,
		main_cur_height   = Height - 8
	}}.

init_color_pairs() ->
	cecho:init_pair(?CPAIR_HEADING,     ?ceCOLOR_RED,    ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_INPUT_BRACK, ?ceCOLOR_BLACK,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_INPUT_DATA,  ?ceCOLOR_WHITE,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_CTRLKEY,     ?ceCOLOR_MAGENTA,?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_CTRLDESCR,   ?ceCOLOR_RED,    ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_OUT_NORMAL,  ?ceCOLOR_WHITE,  ?ceCOLOR_BLACK),
	cecho:init_pair(?CPAIR_OUT_HIGH,    ?ceCOLOR_BLACK,  ?ceCOLOR_WHITE),
	cecho:init_pair(?CPAIR_TSK_RED,     ?ceCOLOR_WHITE,  ?ceCOLOR_RED),
	cecho:init_pair(?CPAIR_TSK_GREEN,   ?ceCOLOR_BLACK,  ?ceCOLOR_GREEN),
	cecho:init_pair(?CPAIR_TSK_BLACK,   ?ceCOLOR_BLACK,  ?ceCOLOR_WHITE),
	cecho:init_pair(?CPAIR_TSK_WHITE,   ?ceCOLOR_WHITE,  ?ceCOLOR_BLUE),
	cecho:init_pair(?CPAIR_TSK_YELLOW,  ?ceCOLOR_BLACK,  ?ceCOLOR_YELLOW),
	cecho:init_pair(?CPAIR_TSK_PURPLE,  ?ceCOLOR_WHITE,  ?ceCOLOR_MAGENTA),
	cecho:init_pair(?CPAIR_TSK_DELAYED, ?ceCOLOR_BLUE,   ?ceCOLOR_BLACK).

window_create_default(Width, H, CY) ->
	{CY + H, cecho:newwin(H, Width, CY, 0)}.

display_title(Width, Wnd, Msg) ->
	TStart = Width - 6 - length(Msg), % 6: "[__]--"
	case TStart > 0 of
	true ->
		Attrs = ?ceA_BOLD bor ?ceCOLOR_PAIR(?CPAIR_HEADING),
		cecho:attron(Wnd, Attrs),
		cecho:whline(Wnd, ?ceACS_HLINE, TStart),
		cecho:mvwaddstr(Wnd, 0, TStart, io_lib:format("[ ~s ]", [Msg])),
		cecho:whline(Wnd, ?ceACS_HLINE, 2),
		cecho:attroff(Wnd, Attrs),
		cecho:wrefresh(Wnd);
	false ->
		nothing
	end.

window_create_input(Width, CY) ->
	{Y1, Wnd} = window_create_default(Width, 1, CY),

	Attrs = ?ceA_BOLD bor ?ceCOLOR_PAIR(?CPAIR_INPUT_BRACK),
	cecho:attron(Wnd, Attrs),
	cecho:mvwaddch(Wnd, 0, 1, $[),
	End = Width - 2,
	cecho:mvwaddch(Wnd, 0, End, $]),
	cecho:attroff(Wnd, Attrs),

	Attrs2 = ?ceA_UNDERLINE bor ?ceCOLOR_PAIR(?CPAIR_INPUT_DATA),
	cecho:attron(Wnd, Attrs2),
	cecho:mvwaddstr(Wnd, 0, 2, lists:duplicate(End - 2, $ )),
	cecho:attroff(Wnd, Attrs2),

	cecho:wrefresh(Wnd),
	{Y1, Wnd}.

window_create_description(Width, CY) ->
	{Y1, Wnd} = window_create_default(Width, 1, CY),
	lists:foreach(fun (Tuple) -> window_draw_description(Wnd, Tuple) end, [
			% -DLY := Hide Delayed, -Sub := Hide Subtasks
			% alt. Tog or t prefix
			{1, ""}, {2, "New"}, {3, ""}, {4, "All"},
			{5, ""}, {6, "Docs"}, {7, "Tasks"}, {8, "-DLY"},
			{9, "-Sub"}, {0, "Quit"}
		]),
	cecho:wrefresh(Wnd),
	{Y1, Wnd}.

window_draw_description(Wnd, {FKey, Msg}) ->
	cecho:wmove(Wnd, 0, (FKey - 1) * 8),
	Atts = ?ceCOLOR_PAIR(?CPAIR_CTRLKEY),
	cecho:attron(Wnd, Atts),
	cecho:waddstr(Wnd, io_lib:format("~2w", [FKey])),
	cecho:attroff(Wnd, Atts),
	Atts2 = ?ceA_BOLD bor ?ceCOLOR_PAIR(?CPAIR_CTRLDESCR),
	cecho:attron(Wnd, Atts2),
	cecho:waddstr(Wnd, io_lib:format("~-6s", [Msg])),
	cecho:attroff(Wnd, Atts2).

handle_cast({getch, Character}, Context) ->
	{noreply, case Character of
		% -- Program Control --
		%?ceKEY_ENTER -> % \n
		16#0a ->
			case Context#view.main_cresult of
			[]          -> Context;
			ResultsList -> edit_page(Context, lists:nth(
						Context#view.main_cur_idx,
						Context#view.main_cresult))
			end;
		?ceKEY_F(2) ->
			% TODO CREATE NEW PAGE
			Context;
		?ceKEY_F(4) ->
			update_filters(Context#view{flt_general = all,
					flt_toplevel = all, flt_delayed = all});
		?ceKEY_F(6) ->
			update_filters(Context#view{flt_general = pages});
		?ceKEY_F(7) ->
			update_filters(Context#view{flt_general = tasks});
		?ceKEY_F(8) ->
			update_filters(Context#view{flt_delayed = toggle(
				Context#view.flt_delayed, current, delayed)});
		?ceKEY_F(9) ->
			update_filters(Context#view{flt_toplevel = toggle(
				Context#view.flt_toplevel, toplevel, subtask)});
		?ceKEY_F(10) ->
			% TODO x Bad that we must unconditionally terminate the
			%        VM. It would be much nicer if we were to do
			%        this conditionally s.t. it only shuts down
			%        after the last application has closed. For now
			%        this hack has to suffice.
			init:stop(0),
			% Other things tried:
			%  - ok = application:stop(d5mantui4) % incomplete?
			%  - ok = application:stop(cecho)     % error but works
			Context;
		?ceKEY_UP ->
			paint_result(Context#view{main_cur_idx = max(1,
					Context#view.main_cur_idx - 1)});
		?ceKEY_DOWN ->
			% TODO x LIMIT CALCULATION WRONG. BOUND BY NUM OF ITEMS DRAWN!
			paint_result(Context#view{main_cur_idx = min(
					length(Context#view.main_cresult),
					Context#view.main_cur_idx + 1)});
		% -- Input Navigation --
		?ceKEY_LEFT ->
			update_cursor(Context#view{main_query_subpos = max(0,
					Context#view.main_query_subpos - 1)});
		?ceKEY_RIGHT ->
			update_cursor(Context#view{main_query_subpos = min(
					length(Context#view.main_query),
					Context#view.main_query_subpos + 1)});
		?ceKEY_END ->
			update_cursor(Context#view{main_query_subpos =
					length(Context#view.main_query)});
		?ceKEY_HOME ->
			update_cursor(Context#view{main_query_subpos = 0});
		?ceKEY_DEL when Context#view.main_query_subpos <
					length(Context#view.main_query) ->
			query_and_draw(delete_character(Context, 0));
		?ceKEY_DEL ->
			Context;
		?ceKEY_BACKSPACE when Context#view.main_query_subpos > 0 ->
			query_and_draw(delete_character(Context, -1));
		?ceKEY_BACKSPACE ->
			Context;
		% -- Character Input --
		_Any ->
			{Prefix, Suffix} = lists:split(
					Context#view.main_query_subpos,
					Context#view.main_query),
			% TODO x WHAT IF WRITING BEYOND LINE LENGTH?
			NewQuery = Prefix ++ [Character|Suffix],
			query_and_draw(update_input(Context#view{
				main_query = NewQuery,
				main_query_subpos =
					Context#view.main_query_subpos + 1}))
		end};
handle_cast({log, Level, Message}, Context) ->
	% TODO x MULTILINE, LINE WRAP, MAX HEIGHT ETC?
	CurIdx = Context#view.main_cur_idx,
	cecho:mvwaddstr(Context#view.wnd_output, CurIdx, 2,
				io_lib:format("[~w] ~s~n", [Level, Message])),
	{noreply, Context#view{main_cur_idx = CurIdx + 1}};
handle_cast({db_loading_complete, TimeMS}, Context) ->
	cecho:mvwaddstr(Context#view.wnd_output, Context#view.main_cur_idx, 2,
			io_lib:format("[ OK ] DB Loading complete after ~wms~n",
			[TimeMS])),
	display_title(Context#view.width, Context#view.wnd_subtitle,
							"Query Results"),
	{noreply, query_and_draw(Context)};
handle_cast(_Cast, Context) ->
	{ok, Context}.

edit_page(Context, Page) ->
	[Executable|Args] = Context#view.command_editor,
	try
		% TODO SUSPEND INPUT REQUIRED PRIOR TO DOING THIS HERE!
		% cecho_srv.erl is of interest here. it spawns an own
		% type of “subprocess” or something. currently there is some
		% sort of inifinite wait for getch(). We could now work on it
		% from two directions:
		%  a) Attempt to suspend the entire cecho stuff by shutting
		%     it down, making it close its port and only after that
		%     shtudown has completed call open_port here. Re-initialize
		%     cecho after returning from open_port
		%     [note that this means re-creating all the windows and stuff?]
		%  b) Attempt to make getch() interruptible by resorting to
		%     doing a low-level call instead of gen_server:call(infinity)
		%     and wait for interrupt signal to be generated by edit
		%     call only to return early from getch(). Would then continue
		%     by “light-stopping” the cecho. This seems to be the version
		%     that is less likely to work correctly due to the dangling
		%     getch on the c-side, though...
		Port = open_port({spawn_executable, Executable}, [{args, Args},
						nouse_stdio, exit_status]),
		% TODO ...
		receive
		{Port, {exit_status, _RC}} -> Context
		end
	catch Etype:Emsg:_StackTrace ->
		% TODO x DEBUG ONLY
		cecho:werase(Context#view.wnd_output),
		cecho:mvwaddstr(Context#view.wnd_output, 1, 2,
			io_lib:format("[FAIL] ~p: ~p~n", [Etype, Emsg])),
		cecho:wrefresh(Context#view.wnd_output),
		timer:sleep(20000),
		cecho:wrefresh(Context#view.wnd_output),
		Context
	end.

update_filters(Context) ->
	Delayed = case Context#view.flt_delayed of
			current -> {8, "=DLY"};
			delayed -> {8, "+DLY"};
			all     -> {8, "-DLY"}
			end,
	Toplevel = case Context#view.flt_toplevel of
			toplevel -> {9, "=Sub"};
			subtask  -> {9, "+Sub"};
			all      -> {9, "-Sub"}
			end,
	window_draw_description(Context#view.wnd_descr, Delayed),
	window_draw_description(Context#view.wnd_descr, Toplevel),
	cecho:wrefresh(Context#view.wnd_descr),
	query_and_draw(Context).

query_and_draw(Context) ->
	RV = try
	case gen_server:call(d5mantui4_db, {query,
				Context#view.main_cur_height * 10,
				Context#view.main_query,
				Context#view.flt_general,
				Context#view.flt_toplevel,
				Context#view.flt_delayed}) of
	{error, Error} ->
		cecho:werase(Context#view.wnd_output),
		cecho:mvwaddstr(Context#view.wnd_output,
			Context#view.main_cur_idx + 1, 2,
			io_lib:format("[FAIL] DB Query failed: ~w~n", [Error])),
		cecho:wrefresh(Context#view.wnd_output),
		Context;
	Results ->
		paint_result(Context#view{main_cresult = Results,
							main_cur_idx = 1})
	end
	catch Etype:Emsg:_StackTrace ->
		% TODO x DEBUG ONLY
		cecho:mvwaddstr(Context#view.wnd_output,
			Context#view.main_cur_idx + 1, 2,
			io_lib:format("[FAIL] ~w: ~p~n", [Etype, Emsg])),
		timer:sleep(20000),
		cecho:wrefresh(Context#view.wnd_output),
		Context
	end,
	RV.

paint_result(Context) ->
	cecho:werase(Context#view.wnd_output),
	draw_pages_recursive(Context, Context#view.main_cresult, 0, 2, 2, 1),
	cecho:wrefresh(Context#view.wnd_output),
	update_cursor(Context).

draw_pages_recursive(_Context, [], _CY, _PW, _CW, EntryNumber) ->
	EntryNumber;
draw_pages_recursive(Context, [Result|Remainder], CY, PW, CW, EntryNumber) ->
	IsSel  = (EntryNumber =:= Context#view.main_cur_idx),
	IsTask = (Context#view.flt_general =:= tasks),
	Attrs  = case {IsTask, IsSel} of
		 {true,  true}  -> ?ceA_BOLD bor
				   priority_to_color(Result#page.task_priority);
		 {true,  false} -> priority_to_color(Result#page.task_priority);
		 {false, true}  -> ?ceCOLOR_PAIR(?CPAIR_OUT_HIGH);
		 {false, false} -> ?ceCOLOR_PAIR(?CPAIR_OUT_NORMAL)
		 end,
	JW = PW + 4 + string:length(binary_to_list(Result#page.name)) +
				(case IsTask of true -> 57; false -> 0 end),
	case (JW > Context#view.width andalso PW =/= 2) of
	true ->
		% discard remainder of pages
		EntryNumber;
	false ->
		cecho:attron(Context#view.wnd_output, Attrs),
		cecho:mvwaddstr(Context#view.wnd_output, CY, PW, case IsTask of
			true -> io_lib:format("~3w ~-14s ~-40s~n",
					[Result#page.section, Result#page.name,
					Result#page.title]);
			false -> io_lib:format("~3w ~s~n", [Result#page.section,
					Result#page.name])
			end),
		cecho:attroff(Context#view.wnd_output, Attrs),
		NW = max(JW, CW),
		{CYS, PWS, CWS} = case CY >= Context#view.main_cur_height - 1 of
			true  -> {0,      NW, NW + 2};
			false -> {CY + 1, PW, NW    }
			end,
		draw_pages_recursive(Context, Remainder, CYS, PWS, CWS,
							EntryNumber + 1)
	end.

priority_to_color("red")     -> ?ceCOLOR_PAIR(?CPAIR_TSK_RED);
priority_to_color("green")   -> ?ceCOLOR_PAIR(?CPAIR_TSK_GREEN);
priority_to_color("black")   -> ?ceCOLOR_PAIR(?CPAIR_TSK_BLACK);
priority_to_color("white")   -> ?ceCOLOR_PAIR(?CPAIR_TSK_WHITE);
priority_to_color("yellow")  -> ?ceCOLOR_PAIR(?CPAIR_TSK_YELLOW);
priority_to_color("purple")  -> ?ceCOLOR_PAIR(?CPAIR_TSK_PURPLE);
priority_to_color("delayed") -> ?ceCOLOR_PAIR(?CPAIR_TSK_DELAYED);
priority_to_color(_Any)      -> ?ceCOLOR_PAIR(?CPAIR_OUT_NORMAL).

update_cursor(Context) ->
	cecho:wmove(Context#view.wnd_input, 0,
					Context#view.main_query_subpos + 2),
	cecho:wrefresh(Context#view.wnd_input),
	Context.

toggle(all,  A, _B) -> A;
toggle(B,   _A,  B) -> all;
toggle(A,    A,  B) -> B.

update_input(Context) ->
	Atts = ?ceA_UNDERLINE bor ?ceCOLOR_PAIR(?CPAIR_INPUT_DATA),
	cecho:attron(Context#view.wnd_input, Atts),
	cecho:mvwaddstr(Context#view.wnd_input, 0, 2, Context#view.main_query),
	cecho:attroff(Context#view.wnd_input, Atts),
	update_cursor(Context).

delete_character(Context, Delta) ->
	{Prefix, [_Drop|Suffix]} = lists:split(Context#view.main_query_subpos +
						Delta, Context#view.main_query),
	NewQuery = Prefix ++ Suffix,
	Atts = ?ceA_UNDERLINE bor ?ceCOLOR_PAIR(?CPAIR_INPUT_DATA),
	cecho:attron(Context#view.wnd_input, Atts),
	cecho:mvwaddstr(Context#view.wnd_input, 0, 2 + length(NewQuery), " "),
	cecho:attroff(Context#view.wnd_input, Atts),
	update_input(Context#view{main_query = NewQuery,
		main_query_subpos = Context#view.main_query_subpos + Delta}).

handle_call(_Query, _From, Context) ->
	{reply, {ok, undefined}, Context}.

handle_info(_Message,    Context)         -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok,      Context}.
