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
    [{handle_event,3}, {init,1}, {help,1}];
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

%% `specevent' is special event type which is not passed to `handle_event' of behaviours
%% Special events currently defined:
%% `eval' -
%%   EvalFun(BhvName) for each behaviour is called (return value ignored but for 
%%   future compatibility it must return either `ok' or `not_handled')
%%   Orig is the event originator (for error logging) and is ignored

handle_event({specevent, {eval, _Orig, EvalFun}, _Irc} = E, State) ->
	Result = (catch EvalFun(State#state.mod)),
	process_event(Result, E, State);
handle_event({Type, Event, Irc} = E, #state{mod = M, data = D} = State) ->
	Result = (catch M:handle_event(Type, Event, Irc#irc{data = D})),
	process_event(Result, E, State);
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

%% Behaviour event handler may return:
%%   `not_handled':  ignored
%%   `ok':           processed
%%   {`ok', Data}:
%%     processed, update state data
%%   {`new_event', Type, Event, Data}:
%%     send new event, update data
%%   {`delayed_event', Delay, Type, Event, Data}:
%%     send new event after Delay msec and update data
%%   exception:      fail
%%   list of anything mentioned above:
%%     each element is processed (possibly generating new events in that order);
%%     data/exception is meaningful in last item only; otherwise ignored.

process_event([Result], Event, State) ->
	process_event(Result, Event, State);
process_event([Result | Rest], Event, State) ->
	process_event(Result, Event, State),
	process_event(Rest, Event, State);
process_event(Result, {_Type, Event, Irc}, #state{mod = M} = State) ->
	case Result of
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
	end.

%% Originator of the event (channel name or nick name) where stack trace or any other log may be sent.
%% `undefined' if N/A.
followup(Event) ->
	case element(2, Event) of
		?USER(Nick)              -> Nick;
		Chan when ?IS_CHAN(Chan) -> Chan;
		Nick when is_list(Nick)  -> Nick;
		undefined                -> undefined
	end.
