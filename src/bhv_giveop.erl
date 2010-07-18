%%%-------------------------------------------------------------------
%%% File    : bhv_giveop.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_giveop).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").
-include(".secret.hrl").

init(_) -> undefined.

%% Only Gods shall know about this service, not mere mortals.
help(_) -> none.

handle_event(cmdevent, {privcmd, ?USER(Nick), [?MAGIC_WORD]}, Irc) ->
	irc_conn:for_each_channel(Irc, fun (Chan) -> irc_conn:mode(Irc, Chan, Nick, "+o") end, Nick),
	ok;
handle_event(_Type, _Event, _Irc) ->
	not_handled.
