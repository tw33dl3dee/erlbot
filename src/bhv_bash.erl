%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_bash).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), [[$# | Rest] | _]}, Irc) ->
	case catch list_to_integer(Rest) of 
		{'EXIT', _} ->
			not_handled;
		Num when Num > 0 ->
			erlbot:bash_quote(Irc, Chan, Num),
			{ok, undefined};
		_ ->
			erlbot:fuckoff(Irc, Chan, Nick),
			{ok, undefined}
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["bash" | Rest]}, Irc) ->
	erlbot:bash_search(Irc, Chan, string:join(Rest, " ")),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
