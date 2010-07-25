%%%-------------------------------------------------------------------
%%% File    : bhv_privcmd.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_privcmd).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(genevent, {privmsg, User, Msg}, _, D) ->
	{new_event, cmdevent, {privcmd, User, erlbot_util:split(Msg)}, D};
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
