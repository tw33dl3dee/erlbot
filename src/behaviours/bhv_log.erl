%%%-------------------------------------------------------------------
%%% File    : bhv_log.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_log).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(Type, Event, _IrcState, _Data) ->
	ok = error_logger:info_report([irc_event, {type, Type}, {event, Event}]).

