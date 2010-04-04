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

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["kickme" | _]}, Irc) ->
	ok = irc_conn:kick(Irc, Chan, Nick, choice:make([["Всегда пожалуйста, ", Nick], 
													 "Кто к нам с хуем придет, тот нахуй и пойдет."]));
handle_event(cmdevent, {chancmd, Chan, _, ["jabberwock"]}, Irc) ->
	jabberwock(Irc, Chan);
handle_event(cmdevent, {chancmd, Chan, _, ["uptime"]}, Irc) ->
	show_uptime(Irc, Chan);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["uuid"]}, Irc) ->
	gen_uuid(Irc, Chan, Nick);
handle_event(cmdevent, {chancmd, Chan, _, ["time"]}, Irc) ->
	{success, [Time]} = util:system("date '+%a %b %d %R:%S %Z %Y'"),
	ok = irc_conn:chanmsg(Irc, Chan, ["Точное время: ", Time, "."]);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["ping"]}, Irc) ->
	ok = irc_conn:command(Irc, choice:make([{chanmsg, Chan, ["Да-да, ", Nick, "?.."]},
											{action, Chan, "понг"},
											{chanmsg, Chan, "Ну, понг."},
											{chanmsg, Chan, [Nick, ": сам пинг, че надо?"]}]));
handle_event(cmdevent, {chancmd, Chan, _, ["dice", Max | _]}, Irc) ->
	case catch list_to_integer(Max) of
		X when X > 0->
			dice(Irc, Chan, X);
		_ ->
			not_handled
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
	ok = irc_conn:chanmsg(Irc, Chan, io_lib:format("Uptime: ~b day(s), ~2..0b:~2..0b:~2..0b", [Day, Hour, Min, Sec])).

-define(DICE_TIMEOUT, 1000).

dice(Irc, Chan, Max) ->
	Res = choice:uniform(Max),
	irc_conn:chanmsg(irc, Chan, "Кручу, верчу, наебать хочу..."),
	timer:sleep(?DICE_TIMEOUT),
	ok = irc_conn:chanmsg(Irc, Chan, integer_to_list(Res)).

-define(JABBERWOCK_FILE, "data/jabberwock.txt").
-define(JABBERWOCK_DELAY, 3000).

jabberwock(Irc, Chan) ->
	irc_conn:chanmsg(Irc, Chan, "Кхм кхм."),
	timer:sleep(?JABBERWOCK_DELAY),
	ok = irc_conn:chanmsg(Irc, Chan, "А вот ХУЙ вам, мне лениво.").

gen_uuid(Irc, Chan, Nick) ->
	{success, [Uuid]} = util:system("uuidgen"),
	ok = irc_conn:chanmsg(Irc, Chan, [Nick, ": ", Uuid]).

%%	{ok, Data} = file:read_file(?JABBERWOCK_FILE),
%%	{ok, Lines} = regexp:split(binary_to_list(Data), "\n"),
%%	lists:foreach(fun (L) -> cmd({chanmsg, [L]), timer:sleep(500) end, Lines}, State),
%%	ok.
