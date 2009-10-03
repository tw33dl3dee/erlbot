-module(bhv2).

-behaviour(irc_behaviour).

-export([handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

handle_event(chanevent, {joined, Chan, _, _}, Conn) ->
	irc_conn:action(Conn, Chan, "1"),
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.
