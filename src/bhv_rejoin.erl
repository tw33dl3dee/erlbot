%%%-------------------------------------------------------------------
%%% File    : bhv_rejoin.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_rejoin).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(customevent, {rejoin, Chan}, Irc) ->
	ok = irc_conn:join(Irc, Chan);
handle_event(_Type, _Event, _Irc) ->
	not_handled.
