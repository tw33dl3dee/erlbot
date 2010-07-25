%%%-------------------------------------------------------------------
%%% File    : bhv_lebedev.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lebedev).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"lynch",						"случайная цитата из линча Лебедева"},
	 {"lynchtopic (lynch topic)",	"случайная цитата из линча Лебедева в топик"}];
help(privcmd) ->
	none;
help(about) ->
	"Линч Лебедева".

handle_event(chanevent, {joined, Chan, ?TOPIC(""), _}, _, _) ->
	lynch(Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynch", "topic" | _]}, _, _) ->
	lynch(Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynchtopic" | _]}, _, _) ->
	lynch(Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynch" | _]}, _, _) ->
	lynch(Chan, message);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

-define(LYNCH_FILE, "priv/data/lynch.txt").

lynch(Chan, Target) ->
	print_lynch(Chan, Target, lynch_message()).

lynch_message() ->
	{ok, Data} = file:read_file(?LYNCH_FILE),
	Lines = string:tokens(utf8:decode(Data), "\r\n"),
	LineNo = choice:uniform(length(Lines)),
	lists:nth(LineNo, Lines).

print_lynch(Chan, topic, Lynch) ->   ok = irc_conn:topic(Chan, Lynch);
print_lynch(Chan, message, Lynch) -> ok = irc_conn:chanmsg(Chan, nohist, Lynch).
