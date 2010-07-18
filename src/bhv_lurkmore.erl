%%%-------------------------------------------------------------------
%%% File    : bhv_lurkmore.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lurkmore).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

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

handle_event(cmdevent, {chancmd, Chan, _, [Cmd | Topic]}, Irc) when Cmd =:= "l" orelse Cmd =:= "л", length(Topic) > 0 ->
	lurkmore_topic(Irc, Chan, string:join(Topic, " "));
handle_event(_Type, _Event, _Irc) ->
	not_handled.

lurkmore_topic(Irc, Chan, Topic) ->
	Url = "http://lurkmore.ru/" ++ erlbot_util:uri_encode(Topic),
	ok = irc_conn:action(Irc, Chan, hist, ["доставил: ", Url]).
