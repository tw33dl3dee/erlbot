-module(bhv4).

-behaviour(irc_behaviour).

-export([handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

handle_event(genevent, {ne_to_slovo, Chan}, Irc) ->
	irc_conn:chanmsg(Irc, Chan, ?U("не то слово")),
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.
