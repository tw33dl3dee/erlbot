-module(bhv3).

-behaviour(irc_behaviour).

-export([handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

handle_event(genevent, {chanmsg, Chan, ?USER(Nick), [1091,1089,1089,1072,1094,1072]}, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Nick ++ ?U(": ога")),
	{new_event, genevent, {ne_to_slovo, Chan}, undefined};
handle_event(_, _, _) ->
	not_handled.
