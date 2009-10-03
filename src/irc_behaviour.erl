%%%-------------------------------------------------------------------
%%% File    : irc_behaviour.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(irc_behaviour).

-behaviour(gen_event).

%% External exports
-export([add_behaviour/2, behaviour_info/1, modules/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {mod     :: atom()}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
add_behaviour(EvMgr, BhvMod) ->
	io:format("adding bhv ~p to ~p~n", [BhvMod, EvMgr]),
	event_sup:start_link(EvMgr, undef, ?MODULE, BhvMod).

behaviour_info(callbacks) ->
    [{handle_event,3}];
behaviour_info(_) ->
    undefined.

modules(BhvMod) ->
	[irc_behaviour, event_sup, BhvMod].

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

init(BhvMod) ->
	io:format("~p: init~n", [BhvMod]),
	{ok, #state{mod = BhvMod}}.

handle_info(_Info, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, nosuchcall, State}.

handle_event({Type, Event, ConnRef}, #state{mod = M} = State) ->
	io:format("forwarding ~p ~p to behaviour ~p~n", [Type, Event, M]),
	case M:handle_event(Type, Event, ConnRef) of
		not_handled ->
			{ok, State};
		{ok, _} ->
			{ok, State};
		{new_event, NewType, NewEvent, _} ->
			gen_event:notify(self(), {NewType, NewEvent, ConnRef})
	end;
handle_event(Event, State) ->
	io:format("unparseable event: ~p~n", [Event]),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------