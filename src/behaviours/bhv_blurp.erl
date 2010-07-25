%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_blurp).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> none.

%% Bot can react to any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, _User, Msg}, _, _) ->
	do_blurp(Chan, Msg);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

-define(BLURP_DELAY, 1000).   % msec

-define(BLURP_GENERIC, [30,   % 1/30th probability of comment
						% use Markov generator
						markov]).  

-define(BLURP_CTX, [["квак|q3", 10,      % 1/10
					 "Пры-ы-ы-ыжка!", "Заперчатили... =(", "Кваку фтопку, Диабла лучше!"],
					["диабл|д2", 5,      % 1/5
					 "Энтерпрайзно сосем!", "А слабо 99-й левел на классике?", "Я таких рун не видел..."],
					["баран!!!", 10,     % 1/10,
					 "Бараны все!!!!!!!!!", "Барааааааааааааааан!!!!", "... и лиса"],
					["спокойн.*ночи", 5, % 1/5
					 "Спокойной ночи, приятных снов, желаю увидеть ослов и козлов!"]]).

do_blurp(Chan, Message) ->
	[RevProb | Words] = match_ctx(Message),
	case choice:make([{1, do}, {RevProb - 1, dont}]) of
		do ->
			timer:sleep(?BLURP_DELAY),
			print_blurp(Chan, Message, Words);
		dont ->
			not_handled
	end.

match_ctx(none)    -> ?BLURP_GENERIC;
match_ctx(Message) -> match_ctx(Message, ?BLURP_CTX).

match_ctx(_, []) ->
	match_ctx(none);
match_ctx(Msg, [[Pattern | Matches] | Rest]) ->
	case erlbot_util:contains(Msg, Pattern) of
		true ->
			Matches;
		false ->
			match_ctx(Msg, Rest)
	end.

print_blurp(Chan, Message, [markov]) ->
	{new_event, customevent, {markov, Chan, Message}, undefined};
print_blurp(Chan, _Message, Words) ->
	ok = irc_conn:chanmsg(Chan, hist, choice:make(Words)).
