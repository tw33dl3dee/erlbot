%%%-------------------------------------------------------------------
%%% File    : bhv_appeal.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_appeal).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

-define(APPEAL_DELAY, 500).

init(_) -> undefined.

handle_event(customevent, {appeal, Chan, ?USER(Nick), Msg}, Irc) ->
	timer:sleep(?APPEAL_DELAY),
	Humiliation = util:contains(Msg, "(хуй|заткни)"),
	Greeting = util:contains(Msg, "превед"),
	FuckOff = util:contains(Msg, "(уебись|сосн?и)"),
	Caress = util:contains(Msg, "(няшка|кавай)"),
	if Humiliation ->
			irc_conn:chanmsg(Irc, Chan, Nick ++ ": хамишь, сцуко."),
			{ok, undefined};
	   Greeting ->
			irc_conn:chanmsg(Irc, Chan, "\\O/ Превед, " ++ Nick ++ "!!!"),
			{ok, undefined};
	   FuckOff ->
			{new_event, customevent, {suicide, Chan, Nick}, undefined};
	   Caress ->
			irc_conn:chanmsg(Irc, Chan, "^_^"),
			{ok, undefined};
	   true ->
			irc_conn:chanmsg(Irc, Chan, "Ня!"),
			{ok, undefined}
	end;
handle_event(_Type, _Event, _Irc) ->
	not_handled.
