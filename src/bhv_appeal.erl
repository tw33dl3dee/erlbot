%%%-------------------------------------------------------------------
%%% File    : bhv_appeal.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_appeal).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").

handle_event(customevent, {appeal, Chan, ?USER(Nick), Msg}, Irc) ->
	Humiliation = util:contains(Msg, "(хуй|заткни)"),
	Greeting = util:contains(Msg, "превед"),
	FuckOff = util:contains(Msg, "(уебись|сосн?и)"),
	Caress = util:contains(Msg, "(няшка|кавай)"),
	if Humiliation ->
			irc_conn:chanmsg(Irc, Chan, Nick ++ ": хамишь, сцуко.");
	   Greeting ->
			irc_conn:chanmsg(Irc, Chan, "\\O/ Превед, " ++ Nick ++ "!!!");
	   %% @TODO move to `bhv_suicide'
	   FuckOff ->
			%erlbot:suicide(Irc, Nick);
			ok;
	   Caress ->
			irc_conn:chanmsg(Irc, Chan, "^_^");
	   true ->
			irc_conn:chanmsg(Irc, Chan, "Ня!")
	end,
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
