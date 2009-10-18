%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_misc).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["kickme" | _]}, Irc) ->
	irc_conn:kick(Irc, Chan, Nick, choice:make([["Всегда пожалуйста, ", Nick], 
												"Кто к нам с хуем придет, тот нахуй и пойдет."])),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["jabberwock"]}, Irc) ->
	erlbot:jabberwock(Irc, Chan),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["uptime"]}, Irc) ->
	erlbot:show_uptime(Irc, Chan),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["time"]}, Irc) ->
	{success, [Time]} = util:system("date '+%a %b %d %R:%S %Z %Y'"),
	irc_conn:chanmsg(Irc, Chan, ["Точное время: ", Time, "."]),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["ping"]}, Irc) ->
	irc_conn:command(Irc, choice:make([{chanmsg, Chan, ["Да-да, ", Nick, "?.."]},
									   {action, Chan, "понг"},
									   {chanmsg, Chan, "Ну, понг."},
									   {chanmsg, Chan, [Nick, ": сам пинг, че надо?"]}])),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["dice", Max | _]}, Irc) ->
	case catch list_to_integer(Max) of
		X when X > 0->
			erlbot:dice(Irc, Chan, X),
			{ok, undefined};
		_ ->
			not_handled
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["identify" | _]}, Irc) ->
	erlbot:identify(Irc, Chan, long);
handle_event(cmdevent, {chancmd, Chan, _, ["id" | _]}, Irc) ->
	erlbot:identify(Irc, Chan, long);
handle_event(cmdevent, {chancmd, Chan, _, ["help" | _]}, Irc) ->
	erlbot:help(Irc, Chan),
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
