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
	 {"##<номер>",		"цитата с Bash.Org"},
	 {"ebash <строка>",	"поиск по цитатам Bash.Org"},
	 {"bash <строка>",	"поиск по цитатам Bash.Org.Ru"}];
help(privcmd) ->
	none;
help(about) -> 
	"Цитаты bash.org и bash.org.ru".

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["##" ++ Rest | _]}, Irc) ->
	bash_quote(Irc, Chan, Nick, Rest, org);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), [[$# | Rest] | _]}, Irc) ->
	bash_quote(Irc, Chan, Nick, Rest, ru);
handle_event(cmdevent, {chancmd, Chan, _, ["bash" | Query]}, Irc) when length(Query) > 0 ->
	bash_search(Irc, Chan, Query, ru);
handle_event(cmdevent, {chancmd, Chan, _, ["ebash" | Query]}, Irc) when length(Query) > 0 ->
	bash_search(Irc, Chan, Query, org);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

bash_quote(Irc, Chan, Nick, NumStr, Domain) ->
	case catch list_to_integer(NumStr) of 
		{'EXIT', _} ->
			not_handled;
		Num when Num > 0 ->
			ok = bhv_common:pipe_script(Irc, Chan, bash_script(Domain), ["-n", integer_to_list(Num)]);
		_ ->
			ok = bhv_common:fuckoff(Irc, Chan, Nick)
	end.

bash_search(Irc, Chan, Query, Domain) ->
	ok = bhv_common:pipe_script(Irc, Chan, bash_script(Domain), ["-s" | Query]).

bash_script(ru)  -> "bash-org-ru.py";
bash_script(org) -> "bash-org.py".
