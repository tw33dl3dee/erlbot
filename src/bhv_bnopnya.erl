%%%-------------------------------------------------------------------
%%% File    : bhv_bnopnya.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : Fixes badly truncated messages which do not end with valid UTF8 sequence.
%%%	              Sadly, many dumb irc clients do not display such messages correctly.
%%% Created :  2 Apr 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_bnopnya).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) ->
	none;
help(privcmd) ->
	none;
help(about) ->
	"Коррекция некорректного обрезания сообщений в UTF8".

handle_event(_, {genmsg, Chan, ?USER(Nick), Msg}, Irc) ->
	check_bnopnya(Chan, Nick, Msg, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

check_bnopnya(Chan, Nick, Msg, Irc) ->
	check_bnopnya(Chan, Nick, Msg, [], Irc).

check_bnopnya(Chan, Nick, [?UTF8_INVALID_SEQ], FixedMsg, Irc) ->
	handle_bnopnya(Chan, Nick, lists:reverse(FixedMsg), Irc);
check_bnopnya(Chan, Nick, [C | Rest], FixedMsg, Irc) ->
	check_bnopnya(Chan, Nick, Rest, [C | FixedMsg], Irc);
check_bnopnya(_, _, [], _, _) ->
	not_handled.

handle_bnopnya(Chan, Nick, FixedMsg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, [Nick, ": бНОПНЯ!!!"]),
	timer:sleep(500),
	ok = irc_conn:chanmsg(Irc, Chan, ["#iconv: ", FixedMsg]).
