%%%-------------------------------------------------------------------
%%% File    : bhv_getop.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_getop).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").
-include(".secret.hrl").

-define(OP_BOT_NICK, "dumbot").

init(_) -> undefined.

help(_) -> none.

handle_event(chanevent, {joined, Chan, _, _}, Irc) ->
	ok = irc_conn:privmsg(Irc, ?OP_BOT_NICK, [?MAGIC_WORD, " ", Chan]);
handle_event(_Type, _Event, _Irc) ->
	not_handled.
