%%%-------------------------------------------------------------------
%%% File    : bhv_rejoin.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_rejoin).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> none.

%% TODO: rejoin when kick by someone
handle_event(customevent, {rejoin, Chan}, _, _) ->
	ok = irc_conn:join(Chan);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
