%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_err_print).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

handle_event(exitevent, {Bhv, Chan, Reason}, Irc) when ?IS_CHAN(Chan) ->
	irc_conn:async_action(Irc, Chan, log(Bhv, Reason)),
	{ok, undefined};
handle_event(exitevent, {Bhv, Nick, Reason}, Irc) when Nick =/= undefined ->
	irc_conn:async_privmsg(Irc, Nick, log(Bhv, Reason)),
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.

log(Bhv, Reason) ->
	util:multiline("CRASHED in behaviour `~p' with reason:~n    ~.8p~n~ts", 
				   [Bhv, Reason, choice:make(["sucks huuuuuge cock :(", "аццки уныл v_v"])]).
