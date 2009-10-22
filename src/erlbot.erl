%%%-------------------------------------------------------------------
%%% File    : erlbot.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot).

-export([blurp/2, show_uptime/2, comment/4, dice/3, bash_quote/3, bash_search/3, empty_check/1,
		 google_search/3, google_calc/3, google_trans/4, lurkmore_topic/3, identify/3,
		 lynch/3, jabberwock/2, fuckoff/3, jbofihe/3, cmafihe/3, jvocuhadju/3, dict/5, help/2]).

-include("utf8.hrl").

empty_check([]) -> [empty_msg()];
empty_check([""]) -> [empty_msg()];
empty_check(S) -> S.

empty_msg() ->
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
	case choice:make([{1, do}, {?BLURP_REV_PROB - 1, dont}]) of
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

-define(COMMENT_REV_PROB, 50).  % 1/100th

comment(topic, Chan, Nick, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, choice:make([["Говенный топег, ", Nick, "."], 
												  "Гг :)", 
												  {8, ["Мощно задвинул, ", Nick, "."]}]));
comment(join, Chan, Nick, Irc) ->
	ok = irc_conn:command(Irc, choice:make([{2, {chanmsg, Chan, ["Превед, ", Nick, "."]}}, 
											{chanmsg, Chan, [">> ВНИМАНИЕ: К нам приходит пользователь СИСЬКИ^W", Nick, 
															 ". Поприветствуем!"]},	
											{action, Chan, ["приветствует ", Nick, "."]}]));
comment(exit, Chan, Nick, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, choice:make([["Нам будет нехватать тебя, ", Nick, "."], 
												  "Гг, наконец-то он ушел."]));
comment(message, Chan, Nick, Irc) ->
    case choice:make([{1, do}, {?COMMENT_REV_PROB - 1, dont}]) of
        do ->
			case choice:make([[neg, "Хуйню спорол, ", Nick, "."], 
							  [pos, Nick, ": лови пиченьку."],
							  [neg, Nick, " -- дятел. ^_^"],
							  [pos, Nick, ", ты гений!"]]) of
				[neg | Msg] ->
					comment(neg, Chan, Nick, Msg, Irc);
				[pos | Msg] ->
					comment(pos, Chan, Nick, Msg, Irc)
			end;
        dont ->
            ok
    end.

-define(SUICIDE_DISABLE_TIMEOUT, 120000).

comment(pos, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Msg),
	{new_event, customevent, {suicide_enable, Nick}, undefined};
comment(neg, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Msg),
	{new_event, customevent, {suicide_disable, Nick, ?SUICIDE_DISABLE_TIMEOUT}, undefined}.

-define(DICE_TIMEOUT, 1000).

dice(Irc, Chan, Max) ->
	Res = choice:uniform(Max),
	irc_conn:chanmsg(irc, Chan, "Кручу, верчу, наебать хочу..."),
	timer:sleep(?DICE_TIMEOUT),
	irc_conn:chanmsg(Irc, Chan, integer_to_list(Res)),
	{ok, undefined}.

-define(SCRIPT_DIR, "scripts").

bash_quote(Irc, Chan, Num) ->
	{success, Lines} = util:execv("bash.pl", [integer_to_list(Num)], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, empty_check(Lines)),
	{ok, undefined}.

bash_search(Irc, Chan, Query) ->
	{success, Lines} = util:execv("bash-search.pl", [Query], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, empty_check(Lines)),
	{ok, undefined}.

google_search(Irc, Chan, Query) ->
	{success, Lines} = util:execv("google.pl", [Query], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, empty_check(Lines)),
	{ok, undefined}.

google_calc(Irc, Chan, Query) ->
	{success, Lines} = util:execv("gcalc.pl", [Query], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, empty_check(Lines)),
	{ok, undefined}.

google_trans(Irc, Chan, Dict, Word) ->
	{success, Lines} = util:execv("gdict.pl", [Word, Dict], ?SCRIPT_DIR, []),
	irc_conn:async_chanmsg(Irc, Chan, empty_check(Lines)),
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
	Lines = string:tokens(utf8:decode(Data), "\r\n"),
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
	irc_conn:chanmsg(Irc, Chan, Nick ++ choice:make([", не еби мне моск.", ", иди нахуй.", ": да хуй тебе!"])).

jbofihe(Irc, Chan, Sentence) ->
	{_, Lines} = util:system("head -n1 | jbofihe -x", [Sentence, $\n]),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

cmafihe(Irc, Chan, Sentence) ->
	{_, Lines} = util:system("head -n1 | cmafihe", [Sentence, $\n]),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

jvocuhadju(Irc, Chan, Words) ->
	{_, Lines} = util:execvp("jvocuhadju", Words),
	irc_conn:async_chanmsg(Irc, Chan, Lines),
	{ok, undefined}.

dict(Irc, Chan, Server, Db, Word) ->
	{success, Lines} = util:execv("dict.rb", ["-h", Server, "-d", Db, Word], ?SCRIPT_DIR),
	irc_conn:async_chanmsg(Irc, Chan, empty_check(Lines)),
	{ok, undefined}.

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
