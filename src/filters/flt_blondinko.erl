%%%-------------------------------------------------------------------
%%% File    : flt_blondinko.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  8 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(flt_blondinko).

-behaviour(erlbot_filter).
-export([init/1, filter_command/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

-define(BLONDINKO_REV_PROB, 100).  %% Chance of message being translated to blondinko style: 1/50th

filter_command({chanmsg, Chan, Save, Msg}, _IrcState, _Data) ->
	case choice:make([{1, do}, {?BLONDINKO_REV_PROB - 1, dont}]) of
        dont -> not_handled;
        do   -> {new_command, {chanmsg, Chan, Save, to_blondinko(Msg)}}
    end;
filter_command(_Command, _IrcState, _Data) ->
	not_handled.

to_blondinko(S) -> to_blondinko(lists:flatten(S), choice:make([true, false])).

to_blondinko([C | Tail], true) ->
	[erlbot_util:sc(C) | to_blondinko(Tail, false)];
to_blondinko([C | Tail], false) ->
	[C | to_blondinko(Tail, true)];
to_blondinko([], _) -> [].
