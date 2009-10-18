%%%-------------------------------------------------------------------
%%% File    : bhv_getop.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_getop).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include(".secret.hrl").

-define(OP_BOT_NICK, "dumbot").

handle_event(chanevent, {joined, Chan, _, _}, Irc) ->
	irc_conn:privmsg(Irc, ?OP_BOT_NICK, [?MAGIC_WORD, " ", Chan]),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
