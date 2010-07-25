%%%-------------------------------------------------------------------
%%% File    : bhv_bnopnya.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : Fixes badly truncated messages which do not end with valid UTF8 sequence.
%%%	              Sadly, many dumb irc clients do not display such messages correctly.
%%% Created :  2 Apr 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_bnopnya).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

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

handle_event(_, {genmsg, Chan, ?USER(Nick), Msg}, _, _) ->
	check_bnopnya(Chan, Nick, Msg);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

check_bnopnya(Chan, Nick, Msg) ->
	check_bnopnya(Chan, Nick, Msg, []).

check_bnopnya(Chan, Nick, [?UTF8_INVALID_SEQ], FixedMsg) ->
	handle_bnopnya(Chan, Nick, lists:reverse(FixedMsg));
check_bnopnya(Chan, Nick, [C | Rest], FixedMsg) ->
	check_bnopnya(Chan, Nick, Rest, [C | FixedMsg]);
check_bnopnya(_, _, [], _) ->
	not_handled.

handle_bnopnya(Chan, Nick, FixedMsg) ->
	irc_conn:chanmsg(Chan, hist, [Nick, ": бНОПНЯ!!!"]),
	timer:sleep(500),
	ok = irc_conn:chanmsg(Chan, hist, ["#iconv: ", FixedMsg]).
