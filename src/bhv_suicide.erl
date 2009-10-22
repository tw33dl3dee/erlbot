%%%-------------------------------------------------------------------
%%% File    : bhv_suicide.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_suicide).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

%% Data is a dict mapping Channel -> {KickerNick, KickTime} and User -> 'disabled'
%% for those who can't command bot to suicide
init(_) -> dict:new().

-define(MAX_SUICIDE_TIME, 60).
-define(DEFAULT_SUICIDE_TIME, 30).

handle_event(chanevent, {joined, Chan, _, _}, Irc) ->
	case dict:find(Chan, Irc#irc.data) of
		{ok, {Kicker, _}} ->
			taunt(Irc, Chan, Kicker),
			{ok, dict:erase(Chan, Irc#irc.data)};
		error ->
			not_handled
	end;
handle_event(customevent, {suicide, Chan, Nick}, Irc) ->
	suicide(Irc, Chan, Nick, ?DEFAULT_SUICIDE_TIME);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["suicide"]}, Irc) ->
	suicide(Irc, Chan, Nick, ?DEFAULT_SUICIDE_TIME);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["suicide", Secs]}, Irc) ->
	case catch list_to_integer(Secs) of 
		{'EXIT', _} ->
			not_handled;
		S when S > 0, S < ?MAX_SUICIDE_TIME->
			suicide(Irc, Chan, Nick, S);
		_ ->
			erlbot:fuckoff(Irc, Chan, Nick),
			ok
	end;
handle_event(chanevent, {kicked, Chan, ?USER(Nick), _}, #irc{nick = Nick, data = Data}) ->
	case dict:find(Chan, Data) of 
		{ok, {_, KickTime}} ->
			{delayed_event, KickTime*1000, customevent, {rejoin, Chan}, Data};
		error ->
			not_handled
	end;
handle_event(customevent, {suicide_disable, Who}, #irc{data = Data}) ->
	{ok, dict:store(Who, disabled, Data)};
handle_event(customevent, {suicide_disable, Who, Timeout}, #irc{data = Data}) ->
	{delayed_event, Timeout, customevent, {suicide_enable, Who}, dict:store(Who, disabled, Data)};
handle_event(customevent, {suicide_enable, all}, #irc{data = Data}) ->
	{ok, dict:filter(fun (K, _) when ?IS_CHAN(K) -> true; 
						 (_, _)                  -> false end, Data)};
handle_event(customevent, {suicide_enable, Who}, #irc{data = Data}) ->
	{ok, dict:erase(Who, Data)};
handle_event(_Type, _Event, _Irc) ->
	not_handled.

suicide(#irc{nick = Nick, data = Data} = Irc, Chan, Kicker, Secs) ->
	case dict:find(Kicker, Data) of
		{ok, disabled} ->
			erlbot:fuckoff(Irc, Chan, Kicker),
			ok;
		error ->
			irc_conn:kick(Irc, Chan, Nick, suicide_reason(Kicker)),
			{ok, dict:store(Chan, {Kicker, Secs}, Data)}
	end.

suicide_reason(Kicker) ->
	choice:make([{2, "Прощай, жестокий мир."},
				 {2, ["Сука ты, ", Kicker, "!"]},
				 {1, "И ты, Брут :("}]).

taunt(Irc, Chan, _Kicker) ->
	irc_conn:command(Irc, choice:make([{2, {action, Chan, "воскрес, аки феникс из пепла"}},
									   {2, {action, Chan, "суицидален"}},
									   {1, {chanmsg, Chan, "Не ждали??? А я тут, суки!!!"}}])).
