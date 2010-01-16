%%%-------------------------------------------------------------------
%%% File    : bhv_help.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Jan 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_help).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) ->
	["!help : вывести вот этот вот бля список команд.",
	 "/msg help : то же самое, но не мозолить глаза остальным обитателям канала"].

handle_event(cmdevent, {chancmd, Chan, _, ["help" | _]}, Irc) ->
	show_help(Chan, Irc);
handle_event(cmdevent, {privcmd, ?USER(Nick), _, ["help" | _]}, Irc) ->
	show_help(Nick, Irc);
handle_event(customevent, {end_help, Target}, Irc) ->
	ok = irc_conn:async_action(Irc, Target, ["няшка =^_^="]);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

show_help(Target, Irc) ->	
	irc_conn:async_action(Irc, Target, ["-- ахуенно полезный и функциональный бот.", "умеет:"]),
	ShowHelp = fun (none)    -> ok; 
				   (MsgList) -> ok = irc_conn:async_action(Irc, Target, tabify(MsgList)) 
			   end,
	[{new_event, specevent,   {eval, Target, help, [], ShowHelp}, undefined},
	 {new_event, customevent, {end_help, Target},                 undefined}].

-define(HELP_TAB_STOP, 4).

tabify(MsgList) ->
	[string:chars($ , ?HELP_TAB_STOP, Msg) || Msg <- MsgList].
