%%%-------------------------------------------------------------------
%%% File    : bhv_wiki.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_wiki).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"w|в <топик>",	"топик из Википедии (англ./рус.)"},
	 {"ww|вв <топик>",	"поиск в Википедии (англ./рус.)"}];
help(privcmd) ->
	none;
help(about) ->
	"Цитирование Википедии".

handle_event(cmdevent, {chancmd, Chan, _, ["w" | Topic]}, _, _) when length(Topic) > 0 ->
	wiki_topic(Chan, "en", Topic);
handle_event(cmdevent, {chancmd, Chan, _, ["в" | Topic]}, _, _) when length(Topic) > 0 ->
	wiki_topic(Chan, "ru", Topic);
handle_event(cmdevent, {chancmd, Chan, _, ["ww" | Topic]}, _, _) when length(Topic) > 0 ->
	wiki_search(Chan, "en", Topic);
handle_event(cmdevent, {chancmd, Chan, _, ["вв" | Topic]}, _, _) when length(Topic) > 0 ->
	wiki_search(Chan, "ru", Topic);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

wiki_topic(Chan, Lang, SearchQuery) ->
	ok = bhv_common:pipe_script(Chan, "wiki.py", ["-l", Lang | SearchQuery]).

wiki_search(Chan, Lang, SearchQuery) ->
	ok = bhv_common:pipe_script(Chan, "wiki.py", ["-l", Lang, "-s" | SearchQuery]).
