%%%-------------------------------------------------------------------
%%% File    : bhv_bash.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_bash).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"#<номер>",		"цитата с Bash.Org.Ru"},
	 {"bash <строка>",	"поиск по цитатам Bash.Org.Ru"}];
help(privcmd) ->
	none;
help(about) -> 
	"Цитаты bash.org.ru".

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), [[$# | Rest] | _]}, Irc) ->
	case catch list_to_integer(Rest) of 
		{'EXIT', _} ->
			not_handled;
		Num when Num > 0 ->
			bash_quote(Irc, Chan, Num);
		_ ->
			bhv_common:fuckoff(Irc, Chan, Nick)
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["bash" | Query]}, Irc) when length(Query) > 0 ->
	bash_search(Irc, Chan, Query);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

bash_quote(Irc, Chan, Num) ->
	ok = bhv_common:pipe_script(Irc, Chan, "bash.py", ["-n", integer_to_list(Num)]).

bash_search(Irc, Chan, Query) ->
	ok = bhv_common:pipe_script(Irc, Chan, "bash.py", ["-s" | Query]).
