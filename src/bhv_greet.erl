%%%-------------------------------------------------------------------
%%% File    : bhv_greet.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_greet).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

%% Will conflict with kick-rejoin
handle_event(chanevent, {joined, Chan, _, _}, Irc) ->
	erlbot:identify(Irc, Chan, short);
handle_event(_Type, _Event, _Irc) ->
	not_handled.
