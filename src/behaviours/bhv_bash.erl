%%%-------------------------------------------------------------------
%%% File    : bhv_bash.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_bash).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

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

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["##" ++ Rest | _]}, _, _) ->
	bash_quote(Chan, Nick, Rest, org);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), [[$# | Rest] | _]}, _, _) ->
	bash_quote(Chan, Nick, Rest, ru);
handle_event(cmdevent, {chancmd, Chan, _, ["bash" | Query]}, _, _) when length(Query) > 0 ->
	bash_search(Chan, Query, ru);
handle_event(cmdevent, {chancmd, Chan, _, ["ebash" | Query]}, _, _) when length(Query) > 0 ->
	bash_search(Chan, Query, org);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

bash_quote(Chan, Nick, NumStr, Domain) ->
	case decode_quote_num(NumStr) of 
		false ->
			not_handled;
		Num when Num > 0 ->
			ok = bhv_common:pipe_script(Chan, bash_script(Domain), ["-n", integer_to_list(Num)]);
		_ ->
			ok = bhv_common:fuckoff(Chan, Nick)
	end.

bash_search(Chan, Query, Domain) ->
	ok = bhv_common:pipe_script(Chan, bash_script(Domain), ["-s" | Query]).

bash_script(ru)  -> "bash-org-ru.py";
bash_script(org) -> "bash-org.py".

decode_quote_num(NumStr) ->
	case catch list_to_integer(NumStr) of 
		{'EXIT', _} ->
			case re:run(NumStr, "[/\\?]([0-9]+)", [unicode, {capture, all_but_first, list}]) of
				nomatch -> false;
				{match, [Num]} -> list_to_integer(Num)
			end;
		Num -> Num
	end.
