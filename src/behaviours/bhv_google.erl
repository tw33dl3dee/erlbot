%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_google).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"gg|гг <строка>",						"поиск через Google REST Services"},
	 {"gc|гк <выражение>",					"вычисление через Google Calculator"},
	 {"gd|гд <выражение>",					"толковый словарь (Google Define)"},
	 {"vs <слово1> <слово2>",				"сравнение популярности (Google Fight)"},
	 {"en-ru|ru-en|de-ru|ru-de <слово>",	"перевод через Google Translate"}];
help(privcmd) ->
	[{"lmgtfy <строка>",                    "тонкая и ненавязчивая реклама Google"}];
help(about) ->
	"Поиск по Google".

handle_event(cmdevent, {chancmd, Chan, _, ["gg" | Query]}, _, _) when length(Query) > 0 ->
	google_search(Chan, "en", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["гг" | Query]}, _, _) when length(Query) > 0 ->
	google_search(Chan, "ru", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["gc" | Query]}, _, _) when length(Query) > 0 ->
	google_calc(Chan, "en", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["гк" | Query]}, _, _) when length(Query) > 0 ->
	google_calc(Chan, "ru", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["gd" | Query]}, _, _) when length(Query) > 0 ->
	google_define(Chan, "en", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["гд" | Query]}, _, _) when length(Query) > 0 ->
	google_define(Chan, "ru", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["vs", Word1, Word2]}, _, _) ->
	google_fight(Chan, "ru", Word1, Word2);
handle_event(cmdevent, {chancmd, Chan, _, [Lang | Words]}, _, _) 
  when Lang =:= "en-ru"; Lang =:= "ru-en"; Lang =:= "de-ru"; Lang =:= "ru-de" ->
	[google_trans(Chan, Lang, Word) || Word <- Words],
	ok;
handle_event(cmdevent, {privcmd, ?USER(Nick), ["lmgtfy" | Query]}, _, _) when length(Query) > 0 ->
	lmgtfy(Nick, Query);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

google_search(Chan, Lang, Query) ->
	ok = bhv_common:pipe_script(Chan, "google.py", ["-l", Lang | Query]).

google_calc(Chan, Lang, Query) ->
	ok = bhv_common:pipe_script(Chan, "google.py", ["-l", Lang, "-c" | Query]).

google_define(Chan, Lang, Query) ->
	ok = bhv_common:pipe_script(Chan, "google.py", ["-l", Lang, "-d" | Query]).

google_fight(Chan, Lang, Word1, Word2) ->
	ok = bhv_common:pipe_script(Chan, "google.py", ["-l", Lang, Word1, "-f", Word2]).

google_trans(Chan, Dict, Word) ->
	ok = bhv_common:pipe_script(Chan, "google.py", ["-D", Dict, Word]).

lmgtfy(Nick, Query) ->
	ok = bhv_common:pipe_script(Nick, "lmgtfy.py", Query).
