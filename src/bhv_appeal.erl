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

handle_event(msgevent, {appeal, Chan, User, Msg}, Irc) ->
	appeal(direct, Chan, User, Msg, Irc);
handle_event(msgevent, {maybe_appeal, Chan, User, Msg}, Irc) ->
	appeal({indirect, chan}, Chan, User, Msg, Irc);
handle_event(cmdevent, {privcmd, ?USER(Nick) = User, Words}, Irc) ->
	appeal({indirect, priv}, Nick, User, string:join(Words, " "), Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

%% From may be user nickname or channel name
%% Type = `direct' | {`indirect', `chan'} | {`indirect', `priv'}
appeal(Type, From, ?USER(Nick) = User, Msg, Irc) ->
	Humiliation = util:contains(Msg, "(суч?ка|хуй|заткни)"),
	Greeting = util:contains(Msg, "превед"),
	FuckOff = util:contains(Msg, "(у?ебись|сосн?и)"),
	Caress = util:contains(Msg, "(няшка|кавай)"),
	if Humiliation ->
			react(Irc, From, [Nick, ": хамишь, сцуко."]);
	   Greeting ->
			react(Irc, From, ["\\O/ Превед, ", Nick, "!!!"]);
	   FuckOff, ?IS_CHAN(From) ->
			{delayed_event, ?APPEAL_DELAY, customevent, {suicide, From, Nick}, undefined};
	   Caress ->
			react(Irc, From, "^_^");
	   Type =:= direct ->
			react(Irc, From, "Ня!");
	   Type =:= {indirect, chan} ->
			%% Induced `genmsg' is `customevent' which means that `msgevent' 
			%% (as `genmsg', `appeal' or `maybe_appeal') occurs once per user message
			{new_event, customevent, {genmsg, From, User, Msg}, undefined};
	   true ->
			not_handled
	end.

react(Irc, From, Msg) ->
	timer:sleep(?APPEAL_DELAY),
	%% @attention Bot will not start smart-appeal because using privmsg here.
	ok = irc_conn:privmsg(Irc, From, Msg).
