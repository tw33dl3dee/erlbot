%%%-------------------------------------------------------------------
%%% File    : bhv_giveop.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_giveop).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4, config_change/2]).

-include("utf8.hrl").
-include("irc.hrl").

init(MagicWord) -> MagicWord.

config_change(MagicWord, _) -> {ok, MagicWord}.

%% Only Gods shall know about this service, not mere mortals.
help(_) -> none.

handle_event(cmdevent, {privcmd, ?USER(Nick), [Magic]}, _, Magic) ->
	irc_conn:for_each_channel(fun (Chan) -> irc_conn:mode(Chan, Nick, "+o") end, Nick),
	ok;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
