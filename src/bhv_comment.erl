%%%-------------------------------------------------------------------
%%% File    : bhv_comment.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_comment).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(genevent, {topic, Chan, User, _}, Conn) ->
	comment(topic, Chan, User, Conn);
handle_event(genevent, {join, User, Chan}, Conn) ->
	comment(join, Chan, User, Conn);
handle_event(genevent, {part, User, Chan, _}, Conn) ->
	comment(exit, Chan, User, Conn);
handle_event(genevent, {quit, User, _}) ->
	comment(exit, "TODO", User, Conn);
handle_event(_Type, _Event, _Conn) ->
	not_handled.
