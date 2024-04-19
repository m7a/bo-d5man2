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

% Idea for new page operation
% ---------------------------
% Store this as a state in view record and then have the lowermost functions
% react to it s.t. cursor handling stuff can remain in place as-is. Use F10 for
% “exit” and maybe assign a key for “back” key. Also create a page under
% construction state record in the view.
%
% struct d5manui_view
% mode transitions
%  error                               -- Displaying an error message
%  loading                             -- DB is loading
%  display                             -- Display Query results
%  [new_task_type, new_task_priority,] -- Special task creation steps
%  new_tags                            -- Keywords entering step
% end mode transitions
-record(view, {mode, height, width, page_template,
		command_editor, new_page_root,
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
		mode              = loading,
		command_editor    = CommandEditor,
		new_page_root     = NewPageRoot,
		height            = Height,
		width             = Width,
		page_template     = #page{},
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
	window_draw_input(Width, window_create_default(Width, 1, CY)).

window_draw_input(Width, {Y1, Wnd}) ->
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
		?ceKEY_F(2) ->
			page_new_start(Context);
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
			Context;
		?ceKEY_UP ->
			paint_result(Context#view{main_cur_idx = max(1,
					Context#view.main_cur_idx - 1)});
		?ceKEY_DOWN ->
			% TODO LIMIT CALCULATION WRONG. BOUND BY NUM OF ITEMS DRAWN! / THIS BUG IS FOR REAL. QUERY SOME TAGS AND THEN SCROLL ALL THE WAY DOWN TO OBSERVE THE CURSOR GOING OFF LIMITS
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
	{noreply, query_and_draw(Context#view{mode = display})};
handle_cast(_Cast, Context) ->
	{ok, Context}.

page_new_start(Context) ->
	case query_to_page_template(Context) of
	{error, Msg} ->
		display_error(Context, Msg);
	Page ->
		query_and_draw(clear_input(Context#view{
			page_template = Page,
			mode = case Page#page.section of
				43 -> new_task_type; _Other -> new_tags
				end
		}))
	end.

clear_input(Context) ->
	cecho:werase(Context#view.wnd_input),
	window_draw_input(Context#view.width, {2, Context#view.wnd_input}),
	cecho:wrefresh(Context#view.wnd_input),
	Context#view{main_query = "", main_query_subpos = 0}.

query_to_page_template(Context) ->
	case string:split(erlang:list_to_binary([string:trim(
				Context#view.main_query)]), " ", all) of
	[<<>>] ->
		{error, "Section prefix required!"};
	[QueryBegin|QueryTail] ->
		case is_number(catch binary_to_integer(QueryBegin)) of
		true ->
			SecInt = binary_to_integer(QueryBegin),
			#page{
				file=filename:join([Context#view.new_page_root,
					QueryBegin,
					string:replace(QueryTail, "/", "_") ++
					case SecInt of
						43     -> ".hot";
						_Other -> ".md"
					end]),
				name=QueryTail,
				section=SecInt
			};
		false -> {error, "Section prefix must be numeric!"}
		end
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
	case Context#view.mode of
	error ->
		Context;
	display ->
		query_and_draw_db(Context, {query,
					Context#view.main_cur_height * 10,
					Context#view.main_query,
					Context#view.flt_general,
					Context#view.flt_toplevel,
					Context#view.flt_delayed});
	new_tags ->
		TagList = string:split(Context#view.main_query, " ", all),
		query_and_draw_db(Context, {query_tags,
					Context#view.main_cur_height * 10,
				case TagList of
				[]     -> <<>>;
				_Other -> list_to_binary(lists:last(TagList))
				end});
	new_task_type ->
		paint_result(Context#view{main_cresult = fake_pages([
				{1, <<"long">>},    {2, <<"short">>},
				{3, <<"subtask">>}, {4, <<"periodic">>}
			]), main_cur_idx = 1});
	new_task_priority ->
		paint_result(Context#view{main_cresult = fake_pages([
				{1, <<"red">>},     {2, <<"green">>},
				{3, <<"black">>},   {4, <<"white">>},
				{5, <<"yellow">>},  {6, <<"purple">>},
				{7, <<"delayed">>}, {8, <<"considered">>}
			]), main_cur_idx = 1})
	end.

query_and_draw_db(Context, Query) ->
	case gen_server:call(d5mantui4_db, Query) of
	{error, Error} ->
		display_error(Context,
			io_lib:format("[FAIL] DB Query: ~w~n", [Error]));
	Results ->
		paint_result(Context#view{main_cresult = Results,
							main_cur_idx = 1})
	end.

display_error(Context, Msg) ->
	Ctx2 = case Context#view.mode of
		error  -> Context#view{main_cur_idx =
					Context#view.main_cur_idx + 1};
		_Other -> Context#view{mode = error, main_cur_idx = 0}
		end,
	cecho:werase(Ctx2#view.wnd_output),
	cecho:mvwaddstr(Ctx2#view.wnd_output, Ctx2#view.main_cur_idx, 2, Msg),
	cecho:wrefresh(Ctx2#view.wnd_output),
	Ctx2.

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
			true  -> io_lib:format("~3w ~-14s ~-40s~n",
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

fake_pages(PageList) ->
	[#page{name=Title, section=Section} || {Section, Title} <- PageList].

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

handle_call({getch, 16#0a}, _From, Context) ->
	{reply, ok, case {Context#view.mode, Context#view.main_cresult} of
		{error, _Any} ->
			query_and_draw(Context#view{mode = display});
		{new_tags, _List} ->
			TPL = Context#view.page_template,
			edit_new(clear_input(Context#view{
				page_template = TPL#page{tags = string:split(
					Context#view.main_query, " ", all)},
				mode = display
			}));
		{_Any2, []} ->
			Context;
		{new_task_type, _List} ->
			progress_new_task(Context, task_type,
							new_task_priority);
		{new_task_priority, _List} ->
			progress_new_task(Context, task_priority, new_tags);
		{display, List} ->
			edit_page(Context, lists:nth(Context#view.main_cur_idx,
									List))
		end
	};
handle_call(_Query, _From, Context) ->
	{reply, error_unknown_call, Context}.

progress_new_task(Context, UpdateField, NextStep) ->
	ResultPage = lists:nth(Context#view.main_cur_idx,
						Context#view.main_cresult),
	FieldValue = binary_to_atom(ResultPage#page.name),
	TPL        = Context#view.page_template,
	query_and_draw(clear_input(Context#view{mode = NextStep,
			page_template =
			case UpdateField of
			task_type     -> TPL#page{task_type     = FieldValue};
			task_priority -> TPL#page{task_priority = FieldValue}
			end
	})).

edit_new(Context) ->
	TPL = Context#view.page_template,
	% Date handling from here
	% https://stackoverflow.com/questions/58004415/how-to-convert-erlangti
	{{Y,M,D}, {H,I,S}} = calendar:now_to_datetime(erlang:timestamp()),
	Tags = lists:join("\", \"", TPL#page.tags),
	case file:write_file(TPL#page.file,
		case TPL#page.section of
		43 -> io_lib:format(
"---
section: 43
x-masysma-name: \"~s\"
title: ~s
date: ~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w
lang: en-US
keywords: [\"~s\", \"task\"]
x-masysma-task-type: ~w
x-masysma-task-priority: ~w
---
Task Overview
=============

This text can be upgraded as needed.

x-masysma-task-type
:   long | short | subtask | periodic
x-masysma-task-priority
:   red | green | black | white | yellow | purple |
    delayed | considered

~w/~2..0w/~2..0w
==========

Task Step. Text intended to show the history and not be changed after addition.
",
				[TPL#page.name, TPL#page.title,
				Y, M, D, H, I, S, Tags, TPL#page.task_type,
				TPL#page.task_priority,
				Y, M, D]
			);
		_RegularPage -> io_lib:format(
"---
section: ~w
x-masysma-name: ~s
title: Untitled
date: ~w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w
lang: en-US
author: [\"Linux-Fan, Ma_Sys.ma (info\@masysma.net)\"]
keywords: [\"~s\"]
x-masysma-version: 1.0.0
x-masysma-repository: https://www.github.com/m7a/...
x-masysma-website: https://masysma.net/~w/~s.xhtml
x-masysma-owned: 1
x-masysma-copyright: (c) ~w Ma_Sys.ma <info\@masysma.net>.
---
Template
========

D5Man 2 Template file.
Edit metadata, delete template, start writing.
",
				[TPL#page.section, TPL#page.name,
				Y, M, D, H, I, S, Tags, TPL#page.section,
				filename:basename(TPL#page.file, ".md"), Y]
			)
		end)
	of
	ok ->
		edit_page(Context, TPL);
	{error, Reason} ->
		display_error(Context,
				io_lib:format("Failed to create page at ~s: ~w",
				[TPL#page.file, Reason]))
	end.

edit_page(Context, Page) ->
	[Executable|Args] = Context#view.command_editor,
	ok = cecho:endwin(),
	Port = open_port({spawn_executable, Executable}, [{args,
		Args ++ [Page#page.file]}, nouse_stdio, exit_status]),
	receive
	{Port, {exit_status, _RC}} ->
		% TODO UPDATE DB AFTER EDIT! [AND DISTINGUISH QUIT AND BACK USER INTENTIONS]
		cecho:refresh(),
		query_and_draw(Context#view{mode=display})
	end.

handle_info(_Message,    Context)         -> {noreply, Context}.
code_change(_OldVersion, Context, _Extra) -> {ok,      Context}.
