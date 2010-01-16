%%%-------------------------------------------------------------------
%%% File    : bhv_wiki.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_wiki).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> 
	["!w|в <топик> : топик из Википедии (англ./рус.)",
	 "!ww|вв <топик> : поиск в Википедии (англ./рус.)"].

handle_event(cmdevent, {chancmd, Chan, _, ["w" | Topic]}, Irc) when length(Topic) > 0 ->
	wiki_topic(Irc, Chan, "en", Topic);
handle_event(cmdevent, {chancmd, Chan, _, ["в" | Topic]}, Irc) when length(Topic) > 0 ->
	wiki_topic(Irc, Chan, "ru", Topic);
handle_event(cmdevent, {chancmd, Chan, _, ["ww" | Topic]}, Irc) when length(Topic) > 0 ->
	wiki_search(Irc, Chan, "en", Topic);
handle_event(cmdevent, {chancmd, Chan, _, ["вв" | Topic]}, Irc) when length(Topic) > 0 ->
	wiki_search(Irc, Chan, "ru", Topic);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

wiki_topic(Irc, Chan, Lang, SearchQuery) ->
	case util:execv("wiki.py", ["-l", Lang | SearchQuery], ?SCRIPT_DIR) of
		{success, Lines} ->
			ok = irc_conn:async_chanmsg(Irc, Chan, bhv_common:empty_check(Lines));
		{{failure, 1}, Trace} ->
			ok = bhv_common:error(Irc, Chan, Trace)
	end.

wiki_search(Irc, Chan, Lang, SearchQuery) ->
	case util:execv("wiki.py", ["-l", Lang, "-s" | SearchQuery], ?SCRIPT_DIR) of
		{success, Lines} ->
			ok = irc_conn:async_chanmsg(Irc, Chan, bhv_common:empty_check(Lines));
		{{failure, 1}, Trace} ->
			ok = bhv_common:error(Irc, Chan, Trace)
	end.
