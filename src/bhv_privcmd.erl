%%%-------------------------------------------------------------------
%%% File    : bhv_privcmd.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_privcmd).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

handle_event(genevent, {privmsg, User, Msg}, _Irc) ->
	{new_event, cmdevent, {privcmd, User, util:split(Msg)}, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
