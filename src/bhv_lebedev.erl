%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lebedev).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

handle_event(chanevent, {joined, Chan, ?TOPIC(""), _}, Irc) ->
	erlbot:lynch(Irc, Chan, topic),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["lynch", "topic" | _]}, Irc) ->
	erlbot:lynch(Irc, Chan, topic),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["lynchtopic" | _]}, Irc) ->
	erlbot:lynch(Irc, Chan, topic),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["lynch" | _]}, Irc) ->
	erlbot:lynch(Irc, Chan, chanmsg),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
