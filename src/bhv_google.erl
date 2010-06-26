%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_google).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"gg|гг <строка>",						"поиск через Google REST Services"},
	 {"gc|гк <выражение>",					"вычисление через Google Calculator"},
	 {"gd|гд <выражение>",					"толковый словарь (Google Define)"},
	 {"vs <слово1> <слово2>",				"сравнение популярности (Google Fight)"},
	 {"en-ru|ru-en|de-ru|ru-de <слово>",	"перевод через Google Translate"}];
help(privcmd) ->
	none;
help(about) ->
	"Поиск по Google".

handle_event(cmdevent, {chancmd, Chan, _, ["gg" | Query]}, Irc) when length(Query) > 0 ->
	google_search(Irc, Chan, "en", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["гг" | Query]}, Irc) when length(Query) > 0 ->
	google_search(Irc, Chan, "ru", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["gc" | Query]}, Irc) when length(Query) > 0 ->
	google_calc(Irc, Chan, "en", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["гк" | Query]}, Irc) when length(Query) > 0 ->
	google_calc(Irc, Chan, "ru", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["gd" | Query]}, Irc) when length(Query) > 0 ->
	google_define(Irc, Chan, "en", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["гд" | Query]}, Irc) when length(Query) > 0 ->
	google_define(Irc, Chan, "ru", Query);
handle_event(cmdevent, {chancmd, Chan, _, ["vs", Word1, Word2]}, Irc) ->
	google_fight(Irc, Chan, "ru", Word1, Word2);
handle_event(cmdevent, {chancmd, Chan, _, [Lang | Words]}, Irc) 
  when Lang =:= "en-ru"; Lang =:= "ru-en"; Lang =:= "de-ru"; Lang =:= "ru-de" ->
	[google_trans(Irc, Chan, Lang, Word) || Word <- Words],
	ok;
handle_event(_Type, _Event, _Irc) ->
	not_handled.

google_search(Irc, Chan, Lang, Query) ->
	ok = bhv_common:pipe_script(Irc, Chan, "google.py", ["-l", Lang | Query]).

google_calc(Irc, Chan, Lang, Query) ->
	ok = bhv_common:pipe_script(Irc, Chan, "google.py", ["-l", Lang, "-c" | Query]).

google_define(Irc, Chan, Lang, Query) ->
	ok = bhv_common:pipe_script(Irc, Chan, "google.py", ["-l", Lang, "-d" | Query]).

google_fight(Irc, Chan, Lang, Word1, Word2) ->
	ok = bhv_common:pipe_script(Irc, Chan, "google.py", ["-l", Lang, Word1, "-f", Word2]).

google_trans(Irc, Chan, Dict, Word) ->
	ok = bhv_common:pipe_script(Irc, Chan, "google.py", ["-D", Dict, Word]).
