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
-export([add_behaviour/2, add_behaviour/3, modules/1]).

%% Meta-information
-export([behaviour_info/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("irc.hrl").

-record(state, {mod     :: atom(),    % Callback module
				data    :: term()}).  % Callback state data

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% Special: define behaviour callbacks
behaviour_info(callbacks) ->
    [{handle_event,3}, {init,1}];
behaviour_info(_) ->
    undefined.

add_behaviour(EvMgr, BhvMod) ->
	event_sup:start_link(EvMgr, undef, ?MODULE, BhvMod).

add_behaviour(EvMgr, BhvMod, BhvArgs) ->
	event_sup:start_link(EvMgr, undef, ?MODULE, {BhvMod, BhvArgs}).

modules(BhvMod) ->
	[irc_behaviour, event_sup, BhvMod].

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

init(BhvMod) when is_atom(BhvMod) ->
	{ok, #state{mod = BhvMod, data = BhvMod:init(undefined)}};
init({BhvMod, BhvArgs}) when is_atom(BhvMod) ->
	{ok, #state{mod = BhvMod, data = BhvMod:init(BhvArgs)}}.

handle_info(_Info, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, nosuchcall, State}.

handle_event({Type, Event, Irc}, #state{mod = M, data = D} = State) ->
	case catch M:handle_event(Type, Event, Irc#irc{data = D}) of
		not_handled ->
			{ok, State};
		ok ->
			{ok, State};
		{ok, Data} ->
			{ok, State#state{data = Data}};
		{new_event, NewType, NewEvent, Data} ->
			gen_event:notify(self(), {NewType, NewEvent, Irc}),
			{ok, State#state{data = Data}};
		{delayed_event, Delay, NewType, NewEvent, Data} ->
			timer:apply_after(Delay, gen_event, notify, [self(), {NewType, NewEvent, Irc}]),
			{ok, State#state{data = Data}};
		{'EXIT', Reason} ->
			gen_event:notify(self(), {exitevent, {M, followup(Event), Reason}, Irc}),
			{'EXIT', Reason};
		Unexpected ->
			Reason = {unexpected_retval, Unexpected},
			gen_event:notify(self(), {exitevent, {M, followup(Event), Reason}, Irc}),
			{'EXIT', Reason}
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

%% Originator of the event (channel name or nick name) where stack trace or any other log may be sent.
%% `undefined' if N/A.
followup(Event) ->
	case element(2, Event) of
		undefined -> undefined;
		?USER(Nick) -> Nick;
		Chan when ?IS_CHAN(Chan) -> Chan;
		Nick -> Nick
	end.
