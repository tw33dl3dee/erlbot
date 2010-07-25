%%%-------------------------------------------------------------------
%%% File    : erlbot_behaviour.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_behaviour).

-behaviour(gen_event).

%% API
-export([start_link/3, modules/1]).

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
    [{handle_event,4}, {init,1}, {help,1}];
behaviour_info(_) ->
    undefined.

start_link(EvMgr, BhvMod, BhvArgs) ->
	event_sup:start_link(EvMgr, ?MODULE, {BhvMod, BhvArgs}).

%% Used in supervisor children specifications
%% BUG: _BhvMod itself won't be listed anywhere
%% (`erlbot_ev' will list `erlbot_behaviour' for all behaviours)
modules(_BhvMod) -> [event_sup].

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

init({BhvMod, BhvArg}) when is_atom(BhvMod) ->
	{ok, #state{mod = BhvMod, data = BhvMod:init(BhvArg)}}.

handle_info(_Info, State) ->
	{ok, State}.

handle_call(_Request, State) ->
	{ok, unknown_call, State}.

%% `specevent' is special event type which is not passed to `handle_event' of behaviours
%% Special events currently defined:
%% `eval' -
%%   EvalFun(BhvName) for each behaviour is called (return value ignored but for 
%%   future compatibility it must return either `ok' or `not_handled')
%%   Orig is the event originator (for error logging) and is ignored

handle_event({specevent, {eval, _Orig, EvalFun}, _} = E, State) ->
	Result = (catch EvalFun(State#state.mod)),
	process_event(Result, E, State);

%% `config_change' is another special event type, sent when behaviour must update 
%%   it's configuration. BhvName:config_change(NewConfig, Data) is called 
%%   (return value may be `not_handled', `ok', {`ok', NewData}).

handle_event({config_change, C, N, R}, State) ->
	case erlbot_config:find_change(State#state.mod, C, N, R) of 
		{changed, _, V2} -> handler_config_change(V2, State);
		{new, V2}        -> handler_config_change(V2, State);
		{removed, _}     -> handler_config_change(undefined, State);
		false            -> {ok, State}
	end;
handle_event({Type, Event, IrcState} = E, #state{mod = M, data = D} = State) ->
	Result = (catch M:handle_event(Type, Event, IrcState, D)),
	process_event(Result, E, State);
handle_event(_Event, State) ->
	error_logger:error_report([{unexpected_irc_event, _Event}]),
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
%%     BUG: data/exception is meaningful in last item only; otherwise ignored.

process_event([Result], Event, State) ->
	process_event(Result, Event, State);
process_event([Result | Rest], Event, State) ->
	process_event(Result, Event, State),
	process_event(Rest, Event, State);
process_event(Result, {_Type, Event, IrcState}, State) ->
	case Result of
		not_handled ->
			{ok, State};
		ok ->
			{ok, State};
		{ok, Data} ->
			{ok, State#state{data = Data}};
		{new_event, NewType, NewEvent, Data} ->
			erlbot_ev:notify({NewType, NewEvent, IrcState}),
			{ok, State#state{data = Data}};
		{delayed_event, Delay, NewType, NewEvent, Data} ->
			erlbot_ev:delayed_notify({NewType, NewEvent, IrcState}, Delay),
			{ok, State#state{data = Data}};
		{'EXIT', Reason} ->
			erlbot_ev:notify({exitevent, {State#state.mod, followup(Event), Reason}, IrcState}),
			{'EXIT', Reason};
		Unexpected ->
			Reason = {unexpected_retval, Unexpected},
			erlbot_ev:notify({exitevent, {State#state.mod, followup(Event), Reason}, IrcState}),
			{'EXIT', Reason}
	end.

handler_config_change(NewConfig, #state{mod = M, data = D} = State) ->
	case erlang:function_exported(M, config_change, 2) of
		false -> error_logger:warning_report([behaviour_config_change_unsupported, {module, M}]),
				 {ok, State};
		true ->
			error_logger:info_report([behaviour_config_change, {module, M}, {arg, NewConfig}]),
			case catch M:config_change(NewConfig, D) of
				not_handled          -> {ok, State};
				ok                   -> {ok, State};
				{ok, Data}           -> {ok, State#state{data = Data}};
				Any                  -> Any
			end
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
