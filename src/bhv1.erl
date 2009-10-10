-module(bhv1).

-behaviour(irc_behaviour).

-export([handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

handle_event(genevent, {chanmsg, Chan, ?USER(Nick), [121,101,115,116,58,32,1093,1091,1081]}, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Nick ++ ?U(": пизда")),
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.
