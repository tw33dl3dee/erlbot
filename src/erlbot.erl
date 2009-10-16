%%%-------------------------------------------------------------------
%%% File    : erlbot.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot).

-export([blurp/2, show_uptime/2, comment/4]).

-include("utf8.hrl").

-define(BLURP_DELAY, 1000).   % msec
-define(BLURP_REV_PROB, 40).  % 1/40th

%% @TODO Move to async process
blurp(Irc, Chan) ->
	Words = ["Хамите", "Хо-хо!", "Знаменито", "Мрак", "Жуть", "Не учите меня жить", 
			 "Как ребёнка", "Кр-р-расота!", "Толстый и красивый", "Поедем на извозчике",
			 "Поедем на таксо", "У вас вся спина белая", "Подумаешь!", "Ого!"],
	case choice:make([{1, do}, {?BLURP_REV_PROB, dont}]) of
		do ->
			irc_conn:chanmsg(Irc, Chan, choice:make(Words)),
			did;
		dont ->
			didnt
	end.

show_uptime(Irc, Chan) ->
	{Uptime, _} = statistics(wall_clock),
	UptimeSec = Uptime div 1000,
	{Rest1, Sec} = {UptimeSec div 60, UptimeSec rem 60},
	{Rest2, Min} = {Rest1 div 60, Rest1 rem 60},
	{Day, Hour} = {Rest2 div 24, Rest2 rem 24},
	irc_conn:chanmsg(Irc, Chan, io_lib:format("Uptime: ~b day(s), ~2..0b:~2..0b:~2..0b", [Day, Hour, Min, Sec])),
	ok.

comment(topic, Chan, Nick, Irc) ->
	irc_conn:chanmsg(Irc, Chan, choice:make([["Говенный топег, ", Nick, "."], 
											 "Гг :)", 
											 {8, ["Мощно задвинул, ", Nick, "."]}])),
	{ok, undefined};
comment(join, Chan, Nick, Irc) ->
	irc_conn:command(Irc, choice:make([{2, {chanmsg, Chan, ["Превед, ", Nick, "."]}}, 
									   {chanmsg, Chan, [">> ВНИМАНИЕ: К нам приходит пользователь СИСЬКИ^W", Nick, ". Поприветствуем!"]},
									   {action, Chan, ["приветствует ", Nick, "."]}])),
	{ok, undefined};
comment(exit, Chan, Nick, Irc) ->
	irc_conn:chanmsg(Irc, Chan, choice:make([["Нам будет нехватать тебя, ", Nick, "."], 
											 "Гг, наконец-то он ушел."])),
	{ok, undefined}.
