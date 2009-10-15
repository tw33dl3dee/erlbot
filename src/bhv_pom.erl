%%%-------------------------------------------------------------------
%%% File    : bhv_pom.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_pom).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(cmdevent, {chancmd, Chan, _, ["mooon"]}, Irc) ->
	{success, Moon} = util:system("pom"),
	irc_conn:chanmsg(Irc, Chan, Moon),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
