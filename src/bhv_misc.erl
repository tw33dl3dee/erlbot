%%%-------------------------------------------------------------------
%%% File    : bhv_misc.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_misc).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["kickme" | _]}, Irc) ->
	irc_conn:kick(Irc, Chan, Nick, choice:make([["Всегда пожалуйста, ", Nick], 
												"Кто к нам с хуем придет, тот нахуй и пойдет."])),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["jabberwock"]}, Irc) ->
	jabberwock(Irc, Chan),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["uptime"]}, Irc) ->
	show_uptime(Irc, Chan),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["time"]}, Irc) ->
	{success, [Time]} = util:system("date '+%a %b %d %R:%S %Z %Y'"),
	irc_conn:chanmsg(Irc, Chan, ["Точное время: ", Time, "."]),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["ping"]}, Irc) ->
	irc_conn:command(Irc, choice:make([{chanmsg, Chan, ["Да-да, ", Nick, "?.."]},
									   {action, Chan, "понг"},
									   {chanmsg, Chan, "Ну, понг."},
									   {chanmsg, Chan, [Nick, ": сам пинг, че надо?"]}])),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["dice", Max | _]}, Irc) ->
	case catch list_to_integer(Max) of
		X when X > 0->
			dice(Irc, Chan, X),
			{ok, undefined};
		_ ->
			not_handled
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["identify" | _]}, Irc) ->
	bhv_common:identify(Irc, Chan, long);
handle_event(cmdevent, {chancmd, Chan, _, ["id" | _]}, Irc) ->
	bhv_common:identify(Irc, Chan, long);
handle_event(cmdevent, {chancmd, Chan, _, ["help" | _]}, Irc) ->
	help(Irc, Chan),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.

show_uptime(Irc, Chan) ->
	{Uptime, _} = statistics(wall_clock),
	UptimeSec = Uptime div 1000,
	{Rest1, Sec} = {UptimeSec div 60, UptimeSec rem 60},
	{Rest2, Min} = {Rest1 div 60, Rest1 rem 60},
	{Day, Hour} = {Rest2 div 24, Rest2 rem 24},
	irc_conn:chanmsg(Irc, Chan, io_lib:format("Uptime: ~b day(s), ~2..0b:~2..0b:~2..0b", [Day, Hour, Min, Sec])),
	ok.

-define(DICE_TIMEOUT, 1000).

dice(Irc, Chan, Max) ->
	Res = choice:uniform(Max),
	irc_conn:chanmsg(irc, Chan, "Кручу, верчу, наебать хочу..."),
	timer:sleep(?DICE_TIMEOUT),
	irc_conn:chanmsg(Irc, Chan, integer_to_list(Res)),
	{ok, undefined}.

-define(JABBERWOCK_FILE, "data/jabberwock.txt").
-define(JABBERWOCK_DELAY, 3000).

jabberwock(Irc, Chan) ->
	irc_conn:chanmsg(Irc, Chan, "Кхм кхм."),
	timer:sleep(?JABBERWOCK_DELAY),
	irc_conn:chanmsg(Irc, Chan, "А вот ХУЙ вам, мне лениво."),
	{ok, undefined}.

%%	{ok, Data} = file:read_file(?JABBERWOCK_FILE),
%%	{ok, Lines} = regexp:split(binary_to_list(Data), "\n"),
%%	lists:foreach(fun (L) -> cmd({chanmsg, [L]), timer:sleep(500) end, Lines}, State),
%%	ok.

-define(CHANCMDLIST, ["    #<номер> : цитата с Bash.Org.Ru",
					  "    bash <строка> : поиск по цитатам Bash.Org.Ru",
					  "    gg <строка> : поиск через Google REST Services",
					  "    gc <выражение> : вычисление через Google Calculator",
					  "    w <topic> : топик из Википедии (англ.)",
					  "    в <топик> : топик из Википедии (рус.)",
					  "    l|л <топик> : ссылка на Луркмор",
					  "    Jbo <sentence> : трансляция с Ложбана",
					  "    jbo <word>: разбор слова с Ложбана",
					  "    jvo <word1> <word2>... : комбинирование слов в lujvo на Ложбане",
					  "    en-jbo|jbo-en|en-ru|ru-en|de-ru|ru-de <слово> : словарный перевод",
					  "    lynch : случайная цитата из линча Лебедева",
					  "    lynchtopic|lynch topic : случайная цитата из линча Лебедева в топик",
					  "    jabberwork : ну, Бармаглот",
					  "    uptime  : аптайм бота",
					  "    time : текущее время",
					  "    ping : пинг бота (хз нах надо)",
					  "    stat : статистика пользователей (-- пока не работает --)",
					  "    dice <число> : бросок кубика (от 1 до N)",
					  "    kickme : оригинальный способ уйти с канала",
					  "    suicide <время> : нихуя не оригинальный способ выразить несогласие с ботом или просто плохое настроение",
					  "    id|identify : дать боту возможность рассказать, кто он такой",
					  "    help : вывести вот этот вот бля список команд."]).

-define(PRIVCMDLIST, ["    hist : история (-- пока не работает --)",
					  "    stat: статистика (-- пока не работает --)"]).

help(Irc, Chan) ->
	irc_conn:async_action(Irc, Chan, ["-- ахуенно полезный и функциональный бот.", "умеет:"]),
	irc_conn:async_chanmsg(Irc, Chan, ["Команды канала:" | ?CHANCMDLIST]),
	irc_conn:async_chanmsg(Irc, Chan, ["Приватные команды:" | ?PRIVCMDLIST]),
	irc_conn:async_action(Irc, Chan, ["няшка =^_^="]),
	ok.
