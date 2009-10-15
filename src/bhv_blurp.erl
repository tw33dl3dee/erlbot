%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_blurp).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(customevent, {genmsg, Chan, _User, _Msg}, Irc) ->
	erlbot:blurp(Irc, Chan),
	{ok, undefined};
handle_event(genevent, {nick, _, NewNick}, Irc) ->
	irc_conn:each_channel(Irc, fun (Chan) -> erlbot:blurp(Irc, Chan) end, NewNick),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
