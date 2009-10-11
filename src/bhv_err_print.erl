%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_err_print).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(exitevent, {Bhv, Reason}, Irc) ->
	Trace = util:print("CRASHED in behaviour `~p' with reason:~n    ~.8p", [Bhv, Reason]),
	irc_conn:each_channel(Irc, fun (Chan) ->
									   irc_conn:action(Irc, Chan, Trace),
									   irc_conn:action(Irc, Chan, "sucks huuuuuge cock :(")
							   end),
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.
