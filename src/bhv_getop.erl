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

handle_event(chanevent, {joined, _, _, _}, Irc) ->
	irc_conn:privmsg(?OP_BOT_NICK, ?GIVEOP_MAGIC_WORD),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
