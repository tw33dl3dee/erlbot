%%%-------------------------------------------------------------------
%%% File    : bhv_err_print.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_err_print).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(exitevent, {Bhv, Chan, Reason}, _, _) when ?IS_CHAN(Chan) ->
	error_logger:error_report([crash, {behaviour, Bhv}, {reason, Reason}]);
	%% ok = irc_conn:bulk_action(Chan, hist, log(Bhv, Reason));
handle_event(exitevent, {Bhv, Nick, Reason}, _, _) when Nick =/= undefined ->
	error_logger:error_report([crash, {behaviour, Bhv}, {reason, Reason}]);
	%% ok = irc_conn:bulk_privmsg(Nick, nohist, log(Bhv, Reason));
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

log(Bhv, Reason) ->
	erlbot_util:multiline("CRASHED in behaviour `~p' with reason:~n    ~.8p~n~ts", 
						  [Bhv, Reason, choice:make(["sucks huuuuuge cock :(", "аццки уныл v_v"])]).
