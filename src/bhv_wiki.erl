%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_wiki).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

handle_event(cmdevent, {chancmd, Chan, _, ["w" | Topic]}, Irc) ->
	erlbot:wiki_topic(Irc, Chan, "en", string:join(Topic, " ")),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["в" | Topic]}, Irc) ->
	erlbot:wiki_topic(Irc, Chan, "ru", string:join(Topic, " ")),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
