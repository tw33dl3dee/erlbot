%%%-------------------------------------------------------------------
%%% File    : bhv_misc.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_misc).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"jabberwork",		"ну, Бармаглот (до сих пор нерабочий. кстати)"},
	 {"uptime ",		"аптайм бота"},
	 {"time",			"текущее время"},
	 {"ping",			"пинг бота (хз нах надо)"},
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

-define(PING_REPLIES(Nick), [{chanmsg, ["Да-да, ", Nick, "?.."]},
							 {action,  "понг"},
							 {chanmsg, "Ну, понг."},
							 {chanmsg, [Nick, ": сам пинг, че надо?"]}]).

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["kickme" | _]}, Irc) ->
	ok = irc_conn:kick(Irc, Chan, Nick, choice:make(?KICK_REASONS(Nick)));
handle_event(cmdevent, {chancmd, Chan, _, ["jabberwock"]}, Irc) ->
	jabberwock(Irc, Chan);
handle_event(cmdevent, {chancmd, Chan, _, ["uptime"]}, Irc) ->
	show_uptime(Irc, Chan);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["uuid"]}, Irc) ->
	gen_uuid(Irc, Chan, Nick);
handle_event(cmdevent, {chancmd, Chan, _, ["time"]}, Irc) ->
	{success, [Time]} = util:system("date '+%a %b %d %R:%S %Z %Y'"),
	ok = irc_conn:chanmsg(Irc, Chan, nohist, ["Точное время: ", Time, "."]);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["ping"]}, Irc) ->
	ok = case choice:make(?PING_REPLIES(Nick)) of 
			 {Action, Msg} -> irc_conn:Action(Irc, Chan, nohist, Msg) 
		 end;
handle_event(cmdevent, {chancmd, Chan, _, ["dice", Max | _]}, Irc) ->
	case catch list_to_integer(Max) of
		X when X > 0 -> roll_dice(Irc, Chan, X);
		_ ->            not_handled
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["identify" | _]}, Irc) ->
	bhv_common:identify(Irc, Chan, long);
handle_event(cmdevent, {chancmd, Chan, _, ["id" | _]}, Irc) ->
	bhv_common:identify(Irc, Chan, long);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

show_uptime(Irc, Chan) ->
	{Uptime, _} = statistics(wall_clock),
	UptimeSec = Uptime div 1000,
	{Rest1, Sec} = {UptimeSec div 60, UptimeSec rem 60},
	{Rest2, Min} = {Rest1 div 60, Rest1 rem 60},
	{Day, Hour} = {Rest2 div 24, Rest2 rem 24},
	ok = irc_conn:chanmsg(Irc, Chan, hist, io_lib:format("Uptime: ~b day(s), ~2..0b:~2..0b:~2..0b", 
														 [Day, Hour, Min, Sec])).

-define(DICE_TIMEOUT, 1000).

roll_dice(Irc, Chan, Max) ->
	Res = choice:uniform(Max),
	irc_conn:chanmsg(irc, Chan, hist, "Кручу, верчу, наебать хочу..."),
	timer:sleep(?DICE_TIMEOUT),
	ok = irc_conn:chanmsg(Irc, Chan, hist, integer_to_list(Res)).

-define(JABBERWOCK_FILE, "data/jabberwock.txt").
-define(JABBERWOCK_DELAY, 3000).

jabberwock(Irc, Chan) ->
	irc_conn:chanmsg(Irc, Chan, hist, "Кхм кхм."),
	timer:sleep(?JABBERWOCK_DELAY),
	ok = irc_conn:chanmsg(Irc, Chan, hist, "А вот ХУЙ вам, мне лениво.").

gen_uuid(Irc, Chan, Nick) ->
	{success, [Uuid]} = util:system("uuidgen"),
	ok = irc_conn:chanmsg(Irc, Chan, hist, [Nick, ": ", Uuid]).
