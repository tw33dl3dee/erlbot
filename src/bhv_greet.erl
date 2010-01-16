%%%-------------------------------------------------------------------
%%% File    : bhv_greet.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_greet).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

%% List of channels already presented ourselves on.
init(_) -> [].

help(_) -> none.

handle_event(chanevent, {joined, Chan, _, _}, Irc) ->
	case lists:member(Chan, Irc#irc.data) of
		true ->
			not_handled;
		false ->
			bhv_common:identify(Irc, Chan, greet),
			{ok, [Chan | Irc#irc.data]}
	end;
handle_event(_Type, _Event, _Irc) ->
	not_handled.
