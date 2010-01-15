%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_google).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

handle_event(cmdevent, {chancmd, Chan, _, ["gg" | Rest]}, Irc) when length(Rest) > 0 ->
	google_search(Irc, Chan, string:join(Rest, " ")),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["gc" | Rest]}, Irc) when length(Rest) > 0 ->
	google_calc(Irc, Chan, string:join(Rest, " ")),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, [Lang | Words]}, Irc) 
  when Lang =:= "en-ru"; Lang =:= "ru-en"; Lang =:= "de-ru"; Lang =:= "ru-de" ->
	Dict = [case C of $- -> $|; C -> C end || C <- Lang],
	[google_trans(Irc, Chan, Dict, Word) || Word <- Words],
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.

google_search(Irc, Chan, Query) ->
	{success, Lines} = util:execv("google.pl", [Query], ?SCRIPT_DIR),
	irc_conn:async_chanmsg(Irc, Chan, bhv_common:empty_check(Lines)),
	{ok, undefined}.

google_calc(Irc, Chan, Query) ->
	{success, Lines} = util:execv("gcalc.pl", [Query], ?SCRIPT_DIR),
	irc_conn:async_chanmsg(Irc, Chan, bhv_common:empty_check(Lines)),
	{ok, undefined}.

google_trans(Irc, Chan, Dict, Word) ->
	{success, Lines} = util:execv("gdict.pl", [Word, Dict], ?SCRIPT_DIR),
	irc_conn:async_chanmsg(Irc, Chan, bhv_common:empty_check(Lines)),
	{ok, undefined}.
