-module(bhv4).

-behaviour(irc_behaviour).

-export([handle_event/3]).

-define(USER(Nick), {Nick, _, _}).

-include("utf8.hrl").

handle_event(genevent, {oops, Chan}, Conn) ->
	irc_conn:chanmsg(Conn, Chan, ?U("не то слово")),
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.
