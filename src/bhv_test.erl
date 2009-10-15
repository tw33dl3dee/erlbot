-module(bhv_test).

-behaviour(irc_behaviour).

-export([handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

handle_event(genevent, {chanmsg, Chan, ?USER(Nick), "@"}, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Nick ++ ": а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5а1б2в3г4д5"),
	1 = 2,
	{ok, undefined};
handle_event(_, _, _) ->
	not_handled.
