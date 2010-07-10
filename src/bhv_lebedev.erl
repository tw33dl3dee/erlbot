%%%-------------------------------------------------------------------
%%% File    : bhv_lebedev.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lebedev).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"lynch",						"случайная цитата из линча Лебедева"},
	 {"lynchtopic (lynch topic)",	"случайная цитата из линча Лебедева в топик"}];
help(privcmd) ->
	none;
help(about) ->
	"Линч Лебедева".

handle_event(chanevent, {joined, Chan, ?TOPIC(""), _}, Irc) ->
	lynch(Irc, Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynch", "topic" | _]}, Irc) ->
	lynch(Irc, Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynchtopic" | _]}, Irc) ->
	lynch(Irc, Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynch" | _]}, Irc) ->
	lynch(Irc, Chan, message);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

-define(LYNCH_FILE, "data/lynch.txt").

lynch(Irc, Chan, Target) ->
	print_lynch(Irc, Chan, Target, lynch_message()).

lynch_message() ->
	{ok, Data} = file:read_file(?LYNCH_FILE),
	Lines = string:tokens(utf8:decode(Data), "\r\n"),
	LineNo = choice:uniform(length(Lines)),
	lists:nth(LineNo, Lines).

print_lynch(Irc, Chan, topic, Lynch) ->   ok = irc_conn:topic(Irc, Chan, Lynch);
print_lynch(Irc, Chan, message, Lynch) -> ok = irc_conn:chanmsg(Irc, Chan, nohist, Lynch).
