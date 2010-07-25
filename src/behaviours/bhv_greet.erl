%%%-------------------------------------------------------------------
%%% File    : bhv_greet.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_greet).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

%% List of channels already presented ourselves on.
init(_) -> [].

help(_) -> none.

handle_event(chanevent, {joined, Chan, _, _}, _, Data) ->
	case lists:member(Chan, Data) of
		true ->
			not_handled;
		false ->
			bhv_common:identify(Chan, greet),
			{ok, [Chan | Data]}
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
