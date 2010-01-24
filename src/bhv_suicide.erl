%%%-------------------------------------------------------------------
%%% File    : bhv_suicide.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_suicide).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

%% Data is a dict mapping Channel -> {KickerNick, KickTime} and User -> 'disabled' | KickPoints
%% where 'disabled' means that User cannot kick bot, KickPoints is user's penalty for kicking bot.
init(_) -> dict:new().

help(chancmd) ->
	[{"suicide <время в сек>", "нихуя не оригинальный способ выразить несогласие с ботом или просто плохое настроение"}];
help(privcmd) ->
	none;
help(about) ->
	"Выражение несогласия с ботом".

-define(MAX_SUICIDE_TIME, 60).
-define(DEFAULT_SUICIDE_TIME, 30).

-define(MAX_KICKS, 2).             % how many subsequent kicks allowed (KickPoints can't exceed `MAX_KICKS'*`POINTS_PER_KICK')
-define(POINTS_PER_KICK, 10).      % kick cost (each `genmsg' decreases KickPoints by 1, each kick increases by this value)
-define(SUICIDE_DISABLE_TIMEOUT, 120000).  % how long user is incapable of kicking bot after exceeding his KickPoints

-define(MAX_KICK_POINTS, ((?MAX_KICKS - 1)*?POINTS_PER_KICK)).

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
			bhv_common:fuckoff(Irc, Chan, Nick),
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
	disable(Who, Data);
handle_event(customevent, {suicide_disable, Who, Timeout}, #irc{data = Data}) ->
	disable(Timeout, Who, Data);
handle_event(customevent, {suicide_enable, all}, #irc{data = Data}) ->
	enable_all(Data);
handle_event(customevent, {suicide_enable, Who}, #irc{data = Data}) ->
	enable(Who, Data);
handle_event(msgevent, {_, _, ?USER(Nick), _}, #irc{data = Data}) ->
	case dict:find(Nick, Data) of
		{ok, KickPoints} when is_integer(KickPoints) ->
			{ok, dict:store(Nick, KickPoints - 1, Data)};
		_ ->
			not_handled
	end;
handle_event(_Type, _Event, _Irc) ->
	not_handled.

suicide(#irc{data = Data} = Irc, Chan, Kicker, Secs) ->
	case dict:find(Kicker, Data) of
		{ok, disabled} ->
			bhv_common:fuckoff(Irc, Chan, Kicker),
			ok;
		{ok, KickPoints} when KickPoints > ?MAX_KICK_POINTS ->
			irc_conn:chanmsg(Irc, Chan, disabled_reason(Kicker)),
			disable(?SUICIDE_DISABLE_TIMEOUT, Kicker, Data);
		{ok, KickPoints} ->
			commit(Irc, Chan, Kicker, Secs, dict:store(Kicker, KickPoints + ?POINTS_PER_KICK, Data));
		error ->
			commit(Irc, Chan, Kicker, Secs, dict:store(Kicker, ?POINTS_PER_KICK, Data))
	end.

commit(#irc{nick = Nick} = Irc, Chan, Kicker, Secs, Data) ->
	irc_conn:kick(Irc, Chan, Nick, suicide_reason(Kicker)),
	{ok, dict:store(Chan, {Kicker, Secs}, Data)}.

enable(Who, Data) ->
	{ok, dict:erase(Who, Data)}.

enable_all(Data) ->	
	{ok, dict:filter(fun (K, _) when ?IS_CHAN(K) -> true; 
						 (_, _)                  -> false end, Data)}.

disable(Who, Data) ->
	{ok, dict:store(Who, disabled, Data)}.

disable(Timeout, Who, Data) ->
	{delayed_event, Timeout, customevent, {suicide_enable, Who}, dict:store(Who, disabled, Data)}.

disabled_reason(Nick) ->
	choice:make([[Nick, ", имей совесть, десу."],
				 [Nick, ", ты не охуел?"],
				 ["Да ебись ты конем, ", Nick],
				 [Nick, ": цыц, бля, разгалделся тут."]]).

suicide_reason(Kicker) ->
	choice:make([{2, "Прощай, жестокий мир."},
				 {2, ["Сука ты, ", Kicker, "!"]},
				 {1, "И ты, Брут :("}]).

taunt(Irc, Chan, _Kicker) ->
	irc_conn:command(Irc, choice:make([{2, {action, Chan, "воскрес, аки феникс из пепла"}},
									   {2, {action, Chan, "суицидален"}},
									   {1, {chanmsg, Chan, "Не ждали??? А я тут, суки!!!"}}])).
