%%%-------------------------------------------------------------------
%%% File    : bhv_comment.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_comment).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

handle_event(chanevent, {topic, Chan, ?USER(Nick), _}, Irc) ->
	erlbot:comment(topic, Chan, Nick, Irc);
handle_event(chanevent, {join, Chan, ?USER(Nick)}, Irc) ->
	erlbot:comment(join, Chan, Nick, Irc);
handle_event(chanevent, {part, Chan, ?USER(Nick), _}, Irc) ->
	erlbot:comment(exit, Chan, Nick, Irc);
%% prevent Nick from being removed from channels FSM _after_ this point
handle_event(genevent, {quit, ?USER(Nick), _}, Irc) ->
	irc_conn:each_channel(Irc, fun (Chan) -> erlbot:comment(exit, Chan, Nick, Irc) end, Nick),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
