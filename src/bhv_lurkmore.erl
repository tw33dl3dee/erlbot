%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lurkmore).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(cmdevent, {chancmd, Chan, _, [Cmd | Topic]}, Irc) when Cmd =:= "l"; Cmd =:= "л" ->
	erlbot:lurkmore_topic(Irc, Chan, string:join(Topic, " ")),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.