%%%-------------------------------------------------------------------
%%% File    : bhv_ping.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  5 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_ping).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

%% Data is map (Nick -> {Time, Channel}) of active user requests
init(_) -> dict:new().

help(chancmd) ->
	[{"ping",			"пинг бота (хз нах надо)"}];
help(privcmd) ->
	none;
help(about) ->
	"Пинг бота".

-define(PING_REPLIES(Nick, RT), [{action,  ["понг (", RT, " сек)"]},
								 {chanmsg, [Nick, ": понг ", RT, " сек"]}]).

-define(PING_DELAY, 500).  %% delay before sending CTCP PING

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["ping"]}, _, Data) ->
	timer:sleep(?PING_DELAY),
	Ts = erlbot_util:epoch(),
	irc_conn:ctcp_request(Nick, ["PING ", integer_to_list(Ts)]),
	{ok, dict:store(Nick, {Ts, Chan}, Data)};
handle_event(genevent, {ctcp_pong, ?USER(Nick), _Ts}, _, Data) ->
	case dict:find(Nick, Data) of
		{ok, {SendTs, Chan}} ->
			RoundTrip = erlbot_util:epoch() - SendTs,
			{Method, Msg} = choice:make(?PING_REPLIES(Nick, integer_to_list(RoundTrip))),
			irc_conn:Method(Chan, hist, Msg);
		error -> not_handled
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
