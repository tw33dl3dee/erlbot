%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_blurp).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> none.

%% Bot can react to any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, _User, Msg}, Irc) ->
	blurp(Irc, Chan, Msg);
handle_event(_, {Event, Chan, _, _, _}, Irc) when Event =:= mode; Event =:= mymode ->
	blurp(Irc, Chan, none);
handle_event(_, {nick, Chan, _, _}, Irc) ->
	blurp(Irc, Chan, none);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

-define(BLURP_DELAY, 1000).   % msec

-define(BLURP_GENERIC, [40,   % 1/40th probability of comment
						"Хамите", "Хо-хо!", "Знаменито", "Мрак", "Жуть", "Не учите меня жить", 
						"Как ребёнка", "Кр-р-расота!", "Толстый и красивый", "Поедем на извозчике",
						"Поедем на таксо", "У вас вся спина белая", "Подумаешь!", "Ого!"]).

-define(BLURP_CTX, [["квак|q3", 10,      % 1/10
					 "Пры-ы-ы-ыжка!", "Заперчатили... =(", "Кваку фтопку, Диабла лучше!"],
					["диабл|д2", 5,      % 1/5
					 "Энтерпрайзно сосем!", "А слабо 99-й левел на классике?", "Я таких рун не видел..."],
					["баран!!!", 10,     % 1/10,
					 "Бараны все!!!!!!!!!", "Барааааааааааааааан!!!!", "... и лиса"],
					["спокойн.*ночи", 5, % 1/5
					 "Спокойной ночи, приятных снов, желаю увидеть ослов и козлов!"]]).

blurp(Irc, Chan, Message) ->
	[RevProb | Words] = match_ctx(Message),
	case choice:make([{1, do}, {RevProb - 1, dont}]) of
		do ->
			timer:sleep(?BLURP_DELAY),
			irc_conn:chanmsg(Irc, Chan, choice:make(Words)),
			ok;
		dont ->
			not_handled
	end.

match_ctx(none) ->
	?BLURP_GENERIC;
match_ctx(Message) ->
	match_ctx(Message, ?BLURP_CTX).

match_ctx(_, []) ->
	match_ctx(none);
match_ctx(Msg, [[Pattern | Matches] | Rest]) ->
	case util:contains(Msg, Pattern) of
		true ->
			Matches;
		false ->
			match_ctx(Msg, Rest)
	end.
