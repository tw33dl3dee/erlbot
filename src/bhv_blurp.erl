%%%-------------------------------------------------------------------
%%% File    : bhv_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_blurp).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

%% Bot can react to any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, _User, _Msg}, Irc) ->
	erlbot:blurp(Irc, Chan),
	{ok, undefined};
handle_event(_, {Event, Chan, _, _, _}, Irc) when Event =:= mode; Event =:= mymode ->
	erlbot:blurp(Irc, Chan),
	{ok, undefined};
handle_event(_, {nick, Chan, _, _}, Irc) ->
	erlbot:blurp(Irc, Chan),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
