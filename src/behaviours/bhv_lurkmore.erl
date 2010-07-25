%%%-------------------------------------------------------------------
%%% File    : bhv_lurkmore.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lurkmore).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"l|л <топик>", "ссылка на Луркмор"}];
help(privcmd) ->
	none;
help(about) ->
	"Ссылка на топик Луркмора".

handle_event(cmdevent, {chancmd, Chan, _, ["l" | Topic]}, _, _) when length(Topic) > 0 ->
	lurkmore_topic(Chan, string:join(Topic, " "));
handle_event(cmdevent, {chancmd, Chan, _, ["л" | Topic]}, _, _) when length(Topic) > 0 ->
	lurkmore_topic(Chan, string:join(Topic, " "));
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

lurkmore_topic(Chan, Topic) ->
	Url = "http://lurkmore.ru/" ++ erlbot_util:uri_encode(Topic),
	ok = irc_conn:action(Chan, hist, ["доставил: ", Url]).
