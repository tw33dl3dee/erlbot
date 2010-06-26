%%%-------------------------------------------------------------------
%%% File    : bhv_err_print.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_err_print).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(exitevent, {Bhv, Chan, Reason}, Irc) when ?IS_CHAN(Chan) ->
	ok = irc_conn:bulk_action(Irc, Chan, hist, log(Bhv, Reason));
handle_event(exitevent, {Bhv, Nick, Reason}, Irc) when Nick =/= undefined ->
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, log(Bhv, Reason));
handle_event(_, _, _) ->
	not_handled.

log(Bhv, Reason) ->
	util:multiline("CRASHED in behaviour `~p' with reason:~n    ~.8p~n~ts", 
				   [Bhv, Reason, choice:make(["sucks huuuuuge cock :(", "аццки уныл v_v"])]).
