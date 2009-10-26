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
	appeal(yes, Chan, Nick, Msg, Irc);
handle_event(customevent, {maybe_appeal, Chan, ?USER(Nick), Msg}, Irc) ->
	appeal(maybe, Chan, Nick, Msg, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

appeal(Prob, Chan, Nick, Msg, Irc) ->
	Humiliation = util:contains(Msg, "(суч?ка|хуй|заткни)"),
	Greeting = util:contains(Msg, "превед"),
	FuckOff = util:contains(Msg, "(уебись|сосн?и)"),
	Caress = util:contains(Msg, "(няшка|кавай)"),
	if Humiliation ->
			react(Irc, Chan, [Nick, ": хамишь, сцуко."]);
	   Greeting ->
			react(Irc, Chan, ["\\O/ Превед, ", Nick, "!!!"]);
	   FuckOff ->
			{delayed_event, ?APPEAL_DELAY, customevent, {suicide, Chan, Nick}, undefined};
	   Caress ->
			react(Irc, Chan, "^_^");
	   Prob =:= yes ->
			react(Irc, Chan, "Ня!");
	   true ->
			not_handled
	end.

react(Irc, Chan, Msg) ->
	timer:sleep(?APPEAL_DELAY),
	ok = irc_conn:chanmsg(Irc, Chan, Msg).
