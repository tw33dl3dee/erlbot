%%%-------------------------------------------------------------------
%%% File    : bhv_misc.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_misc).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"jabberwork",		"ну, Бармаглот (до сих пор нерабочий. кстати)"},
	 {"uptime ",		"аптайм бота"},
	 {"time",			"текущее время"},
	 {"dice <число>",	"бросок кубика (от 1 до N)"},
	 {"kickme",			"оригинальный способ уйти с канала"},
	 {"uuid",           "сгенерировать UUID (уникальный, под ответственность бота)"},
	 {"id (identify)",	"дать боту возможность рассказать, кто он такой"}];
help(privcmd) ->
	none;
help(about) ->
	"Всякая разнотень".

-define(KICK_REASONS(Nick), [["Всегда пожалуйста, ", Nick], 
							 "Кто к нам с хуем придет, тот нахуй и пойдет."]).

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["kickme" | _]}, _, _) ->
	ok = irc_conn:kick(Chan, Nick, choice:make(?KICK_REASONS(Nick)));
handle_event(cmdevent, {chancmd, Chan, _, ["jabberwock"]}, _, _) ->
	jabberwock(Chan);
handle_event(cmdevent, {chancmd, Chan, _, ["uptime"]}, _, _) ->
	show_uptime(Chan);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["uuid"]}, _, _) ->
	gen_uuid(Chan, Nick);
handle_event(cmdevent, {chancmd, Chan, _, ["time"]}, _, _) ->
	{success, [Time]} = erlbot_util:system("date '+%a %b %d %R:%S %Z %Y'"),
	ok = irc_conn:chanmsg(Chan, nohist, ["Точное время: ", Time, "."]);
handle_event(cmdevent, {chancmd, Chan, _, ["dice", Max | _]}, _, _) ->
	case catch list_to_integer(Max) of
		X when X > 0 -> roll_dice(Chan, X);
		_ ->            not_handled
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["identify" | _]}, _, _) ->
	identify(Chan);
handle_event(cmdevent, {chancmd, Chan, _, ["id" | _]}, _, _) ->
	identify(Chan);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

show_uptime(Chan) ->
	{Uptime, _} = statistics(wall_clock),
	UptimeSec = Uptime div 1000,
	{Rest1, Sec} = {UptimeSec div 60, UptimeSec rem 60},
	{Rest2, Min} = {Rest1 div 60, Rest1 rem 60},
	{Day, Hour} = {Rest2 div 24, Rest2 rem 24},
	ok = irc_conn:chanmsg(Chan, hist, io_lib:format("Uptime: ~b day(s), ~2..0b:~2..0b:~2..0b", 
														 [Day, Hour, Min, Sec])).

-define(DICE_TIMEOUT, 1000).

roll_dice(Chan, Max) ->
	Res = choice:uniform(Max),
	irc_conn:chanmsg(Chan, hist, "Кручу, верчу, наебать хочу..."),
	timer:sleep(?DICE_TIMEOUT),
	ok = irc_conn:chanmsg(Chan, hist, integer_to_list(Res)).

-define(JABBERWOCK_FILE, "data/jabberwock.txt").
-define(JABBERWOCK_DELAY, 3000).

jabberwock(Chan) ->
	irc_conn:chanmsg(Chan, hist, "Кхм кхм."),
	timer:sleep(?JABBERWOCK_DELAY),
	ok = irc_conn:chanmsg(Chan, hist, "А вот ХУЙ вам, мне лениво.").

gen_uuid(Chan, Nick) ->
	{success, [Uuid]} = erlbot_util:system("uuidgen"),
	ok = irc_conn:chanmsg(Chan, hist, [Nick, ": ", Uuid]).


version_digit($0) -> "ноль";
version_digit($1) -> "один";
version_digit($2) -> "два";
version_digit($3) -> "три";
version_digit($4) -> "четыре";
version_digit($5) -> "пять";
version_digit($6) -> "шесть";
version_digit($7) -> "семь";
version_digit($8) -> "восемь";
version_digit($9) -> "девять";
version_digit(_)  -> "".

version_to_string(Vsn) -> 
	[version_digit(D) || D <- Vsn].

-define(BUGTRACK_URL, "http://redmine.tweedle-dee.org/projects/erlbot/issues/new").

identify(Chan) ->
	[Vsn] = [Vsn || {erlbot, _, Vsn} <- application:which_applications()],
	irc_conn:action(Chan, nohist, ["ня", version_to_string(Vsn)]),
	timer:sleep(500),
	irc_conn:action(Chan, nohist, choice:make(["векторен и гипертекстов",
											   "металлическ и блестящ",
											   "готичен и православен"])),
	ok = irc_conn:chanmsg(Chan, nohist, ["Советы и предложения постить сюды: ", 
										 ?BUGTRACK_URL]).
