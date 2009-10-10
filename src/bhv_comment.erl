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

handle_event(genevent, {topic, Chan, User, _}, Irc) ->
	comment(topic, Chan, User, Irc);
handle_event(genevent, {join, User, Chan}, Irc) ->
	comment(join, Chan, User, Irc);
handle_event(genevent, {part, User, Chan, _}, Irc) ->
	comment(exit, Chan, User, Irc);
handle_event(genevent, {quit, User, _}, Irc) ->
	comment(exit, "TODO", User, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.
