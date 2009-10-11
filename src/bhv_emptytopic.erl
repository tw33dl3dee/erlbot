%%%-------------------------------------------------------------------
%%% File    : bhv_emptytopic.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_emptytopic).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(chanevent, {joined, Chan, ?TOPIC(""), _}, Irc) ->
	erlbot:lynch(topic, Chan, Irc),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
