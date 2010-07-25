%%%-------------------------------------------------------------------
%%% File    : bhv_getop.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_getop).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4, config_change/2]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

-define(OP_BOT_NICK, "dumbot").

init(MagicWord) -> MagicWord.

config_change(MagicWord, _) -> {ok, MagicWord}.

help(_) -> none.

handle_event(chanevent, {joined, Chan, _, _}, _, Magic) ->
	ok = irc_conn:privmsg(?OP_BOT_NICK, nohist, [Magic, " ", Chan]);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
