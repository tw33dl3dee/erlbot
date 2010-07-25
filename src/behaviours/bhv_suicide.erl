%%%-------------------------------------------------------------------
%%% File    : bhv_suicide.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_suicide).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

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
-define(SUICIDE_DISABLE_TIMEOUT, 120000).  % how much time user is incapable of kicking bot after exceeding his KickPoints

-define(MAX_KICK_POINTS, ((?MAX_KICKS - 1)*?POINTS_PER_KICK)).

handle_event(chanevent, {joined, Chan, _, _}, _, Data) ->
	case dict:find(Chan, Data) of
		{ok, {Kicker, _}} ->
			taunt(Chan, Kicker),
			{ok, dict:erase(Chan, Data)};
		error ->
			not_handled
	end;
handle_event(customevent, {suicide, Chan, Nick}, IrcState, Data) ->
	suicide(Chan, Nick, ?DEFAULT_SUICIDE_TIME, IrcState, Data);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["suicide"]}, IrcState, Data) ->
	suicide(Chan, Nick, ?DEFAULT_SUICIDE_TIME, IrcState, Data);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["suicide", Secs]}, IrcState, Data) ->
	case catch list_to_integer(Secs) of 
		{'EXIT', _} ->
			not_handled;
		S when S > 0, S < ?MAX_SUICIDE_TIME->
			suicide(Chan, Nick, S, IrcState, Data);
		_ ->
			bhv_common:fuckoff(Chan, Nick),
			ok
	end;
handle_event(chanevent, {kicked, Chan, ?USER(Nick), _}, #irc_state{nick = Nick}, Data) ->
	case dict:find(Chan, Data) of 
		{ok, {_, KickTime}} ->
			{delayed_event, KickTime*1000, customevent, {rejoin, Chan}, Data};
		error ->
			not_handled
	end;
handle_event(customevent, {suicide_disable, Who}, _, Data) ->
	disable(Who, Data);
handle_event(customevent, {suicide_disable, Who, Timeout}, _, Data) ->
	disable(Timeout, Who, Data);
handle_event(customevent, {suicide_enable, all}, _, Data) ->
	enable_all(Data);
handle_event(customevent, {suicide_enable, Who}, _, Data) ->
	enable(Who, Data);
handle_event(msgevent, {_, _, ?USER(Nick), _}, _, Data) ->
	case dict:find(Nick, Data) of
		{ok, KickPoints} when is_integer(KickPoints) ->
			{ok, dict:store(Nick, KickPoints - 1, Data)};
		_ ->
			not_handled
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

suicide(Chan, Kicker, Secs, IrcState, Data) ->
	case dict:find(Kicker, Data) of
		{ok, disabled} ->
			ok = bhv_common:fuckoff(Chan, Kicker);
		{ok, KickPoints} when KickPoints > ?MAX_KICK_POINTS ->
			irc_conn:chanmsg(Chan, hist, disabled_reason(Kicker)),
			disable(?SUICIDE_DISABLE_TIMEOUT, Kicker, Data);
		{ok, KickPoints} ->
			commit(Chan, Kicker, Secs, IrcState, 
				   dict:store(Kicker, KickPoints + ?POINTS_PER_KICK, Data));
		error ->
			commit(Chan, Kicker, Secs, IrcState, 
				   dict:store(Kicker, ?POINTS_PER_KICK, Data))
	end.

commit(Chan, Kicker, Secs, #irc_state{nick = Nick}, Data) ->
	irc_conn:kick(Chan, Nick, suicide_reason(Kicker)),
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

-define(TAUNT_REPLIES, [{2, {action,  "воскрес, аки феникс из пепла"}},
						{2, {action,  "суицидален"}},
						{1, {chanmsg, "Не ждали??? А я тут, суки!!!"}}]).

taunt(Chan, _Kicker) ->
	case choice:make(?TAUNT_REPLIES) of
		{Action, Msg} -> ok = irc_conn:Action(Chan, nohist, Msg)
	end.
