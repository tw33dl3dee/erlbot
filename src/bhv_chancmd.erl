%%%-------------------------------------------------------------------
%%% File    : bhv_chancmd.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_chancmd).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(genevent, {chanmsg, Chan, User, [$! | Cmd]}, _Conn) ->
	{new_event, customevent, {chancmd, Chan, User, util:split(Cmd)}, undefined};
handle_event(_Type, _Event, _Conn) ->
	not_handled.
