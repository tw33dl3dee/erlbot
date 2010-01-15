%%%-------------------------------------------------------------------
%%% File    : bhv_lebedev.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lebedev).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

handle_event(chanevent, {joined, Chan, ?TOPIC(""), _}, Irc) ->
	lynch(Irc, Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynch", "topic" | _]}, Irc) ->
	lynch(Irc, Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynchtopic" | _]}, Irc) ->
	lynch(Irc, Chan, topic);
handle_event(cmdevent, {chancmd, Chan, _, ["lynch" | _]}, Irc) ->
	lynch(Irc, Chan, chanmsg);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

-define(LYNCH_FILE, "data/lynch.txt").

lynch(Irc, Chan, Action) ->
	{ok, Data} = file:read_file(?LYNCH_FILE),
	Lines = string:tokens(utf8:decode(Data), "\r\n"),
	LineNo = choice:uniform(length(Lines)),
	ok = irc_conn:command(Irc, {Action, Chan, lists:nth(LineNo, Lines)}).
