%%%-------------------------------------------------------------------
%%% File    : erlbot_filter.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  7 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_filter).

-behaviour(gen_event).

%% API
-export([start_link/2, modules/1]).

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
    [{filter_command,3}, {init,1}];
behaviour_info(_) ->
    undefined.

start_link(EvMgr, FltMod) ->
	%% Pass filter module name as handler Id
	event_sup:start_link(EvMgr, {?MODULE, FltMod}, FltMod).

%% Used in supervisor children specifications
%% BUG: _FltMod itself won't be listed anywhere
%% (`erlbot_ev' will list `erlbot_filter' for all filters)
modules(_FltMod) -> [event_sup].

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

init(FltMod) when is_atom(FltMod) ->
    FltConfig = erlbot_config:get_value(FltMod),
	{ok, #state{mod = FltMod, data = FltMod:init(FltConfig)}}.

handle_info(_Info, State) ->
	{ok, State}.

handle_call({Command, IrcState}, #state{mod = M, data = D} = State) ->
	Result = (catch M:filter_command(Command, IrcState, D)),
	process_result(Result, Command, IrcState, State).

%% TODO: handle config_change
handle_event({config_change, _C, _N, _R}, State) ->
	{ok, State};
handle_event(_Event, State) ->
	error_logger:warning_report([{unexpected_event, _Event}]),
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

process_result(Result, Command, IrcState, State) ->
	case Result of
		not_handled ->
			{ok, {Command, IrcState}, State};
		ok ->
			{ok, {Command, IrcState}, State};
		{ok, NewData} ->
			{ok, Command, State#state{data = NewData}};
		{new_command, NewCommand} ->
			{ok, {NewCommand, IrcState}, State};
		{new_command, NewCommand, NewData} ->
			{ok, {NewCommand, IrcState}, State#state{data = NewData}};
		{'EXIT', Reason} ->
			erlbot_ev:notify({exitevent, {State#state.mod, followup(Command), Reason}, IrcState}),
			{'EXIT', Reason};
		Unexpected ->
			Reason = {unexpected_retval, Unexpected},
			erlbot_ev:notify({exitevent, {State#state.mod, followup(Command), Reason}, IrcState}),
			{'EXIT', Reason}
	end.

%% Command target (channel name or nick name) where stack trace or any other log may be sent.
%% `undefined' if N/A.
followup(_Command) ->
	undefined.
