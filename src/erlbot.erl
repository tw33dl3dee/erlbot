%%%-------------------------------------------------------------------
%%% File    : erlbot.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot).

-export([blurp/2, show_uptime/2, comment/4, dice/3, bash_quote/3, bash_search/3, 
		 google_search/3, google_calc/3, google_trans/4, lurkmore_topic/3, identify/3,
		lynch/3, jabberwock/2, fuckoff/3]).

-include("utf8.hrl").

empty_msg_check([]) -> empty_error_msg();
empty_msg_check([""]) -> empty_error_msg();
empty_msg_check(S) -> S.

empty_error_msg() ->
	choice:make(["А вот хуй...",
				 "<тут могла бы быть ваша реклама>",
				 "Да хер его знает.",
				 "Почувствуйте себя неудачником!"]).

-define(BLURP_DELAY, 1000).   % msec
-define(BLURP_REV_PROB, 40).  % 1/40th

blurp(Irc, Chan) ->
	Words = ["Хамите", "Хо-хо!", "Знаменито", "Мрак", "Жуть", "Не учите меня жить", 
			 "Как ребёнка", "Кр-р-расота!", "Толстый и красивый", "Поедем на извозчике",
			 "Поедем на таксо", "У вас вся спина белая", "Подумаешь!", "Ого!"],
	case choice:make([{1, do}, {?BLURP_REV_PROB, dont}]) of
		do ->
			timer:sleep(?BLURP_DELAY),
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

-define(DICE_TIMEOUT, 1000).

dice(Irc, Chan, Max) ->
	Res = choice:uniform(Max),
	irc_conn:chanmsg(irc, Chan, "Кручу, верчу, наебать хочу..."),
	timer:sleep(?DICE_TIMEOUT),
	irc_conn:chanmsg(Irc, Chan, integer_to_list(Res)),
	{ok, undefined}.

-define(SCRIPT_DIR, "scripts").

bash_quote(Irc, Chan, Num) ->
	{success, Lines} = util:execvp("bash.pl", [integer_to_list(Num)], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

bash_search(Irc, Chan, Query) ->
	{success, Lines} = util:execvp("bash-search.pl", [Query], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

google_search(Irc, Chan, Query) ->
	{success, Lines} = util:execvp("google.pl", [Query], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

google_calc(Irc, Chan, Query) ->
	{success, Lines} = util:execvp("gcalc.pl", [Query], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

google_trans(Irc, Chan, Dict, Word) ->
	{success, Lines} = util:execvp("gdict.pl", [Word, Dict], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

lurkmore_topic(Irc, Chan, Topic) ->
	Url = "http://lurkmore.ru/" ++ util:uri_encode(Topic),
	irc_conn:action(Irc, Chan, ["доставил: ", Url]),
	{ok, undefined}.

identify(Irc, Chan, short) ->
	irc_conn:action(Irc, Chan, "нядваноль"),
	timer:sleep(500),
	irc_conn:action(Irc, Chan, choice:make(["векторен и гипертекстов",
											"металлическ и блестящ"])),
	{ok, undefined};
identify(Irc, Chan, long) ->
	identify(Irc, Chan, short),
	irc_conn:action(Irc, Chan, ["обитает по адресу: ", "http://tweedle-dee.org/bzr/erlbot/"]),
	{ok, undefined}.

-define(LYNCH_FILE, "data/lynch.txt").

lynch(Irc, Chan, Action) ->
	{ok, Data} = file:read_file(?LYNCH_FILE),
	Lines = string:tokens(utf8:decode(Data), "\n"),
	LineNo = choice:uniform(length(Lines)),
	irc_conn:command(Irc, {Action, Chan, lists:nth(LineNo, Lines)}),
	{ok, undefined}.

-define(JABBERWOCK_FILE, "data/jabberwock.txt").
-define(JABBERWOCK_DELAY, 3000).

jabberwock(Irc, Chan) ->
	irc_conn:chanmsg(Irc, Chan, "Кхм кхм."),
	timer:sleep(?JABBERWOCK_DELAY),
	irc_conn:chanmsg(Irc, Chan, "А вот ХУЙ вам, мне лениво."),
	{ok, undefined}.

fuckoff(Irc, Chan, Nick) ->
	irc_conn:chanmsg(Irc, Chan, Nick ++ choice:make([", не еби мне моск", ", иди нахуй", ": да хуй тебе"])).
