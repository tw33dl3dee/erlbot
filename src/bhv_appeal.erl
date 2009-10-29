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
handle_event(customevent, {maybe_appeal, From, ?USER(Nick), Msg}, Irc) ->
	appeal(maybe, From, Nick, Msg, Irc);
handle_event(cmdevent, {privcmd, ?USER(Nick) = User, Words}, _Irc) ->
	{new_event, customevent, {maybe_appeal, Nick, User, string:join(Words, " ")}, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.

% From may be user nickname or channel name
appeal(Prob, From, Nick, Msg, Irc) ->
	Humiliation = util:contains(Msg, "(суч?ка|хуй|заткни)"),
	Greeting = util:contains(Msg, "превед"),
	FuckOff = util:contains(Msg, "(уебись|сосн?и)"),
	Caress = util:contains(Msg, "(няшка|кавай)"),
	if Humiliation ->
			react(Irc, From, [Nick, ": хамишь, сцуко."]);
	   Greeting ->
			react(Irc, From, ["\\O/ Превед, ", Nick, "!!!"]);
	   FuckOff, ?IS_CHAN(From) ->
			{delayed_event, ?APPEAL_DELAY, customevent, {suicide, From, Nick}, undefined};
	   Caress ->
			react(Irc, From, "^_^");
	   Prob =:= yes ->
			react(Irc, From, "Ня!");
	   true ->
			not_handled
	end.

react(Irc, From, Msg) ->
	timer:sleep(?APPEAL_DELAY),
	%% @attention Bot will not start smart-appeal because using privmsg here.
	ok = irc_conn:privmsg(Irc, From, Msg).
