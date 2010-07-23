%%%-------------------------------------------------------------------
%%% File    : event_sup.erl
%%% Author  : Ivan Korotkov <nkik@niisi.ras.ru>
%%% Description : 
%%%
%%% Created : 20 Jul 2010 by Ivan Korotkov <nkik@niisi.ras.ru>
%%%-------------------------------------------------------------------
-module(event_sup).

-behaviour(gen_server).

%% API
-export([start_link/4, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
start_link(EventMgr, Module, Args) ->
    gen_server:start_link(?MODULE, {EventMgr, Module, Args}, []).

start_link(Name, EventMgr, Module, Args) ->
    gen_server:start_link(Name, ?MODULE, {EventMgr, Module, Args}, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({EventMgr, Module, Args}) ->
    ok = gen_event:add_sup_handler(EventMgr, Module, Args),
	erlang:monitor(process, EventMgr),
    {ok, {EventMgr, Module}}.

handle_call(_Req, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Req, State) ->
	{noreply, State}.

handle_info({gen_event_EXIT, Module, Reason}, State) ->
	error_logger:error_report([{gen_event_EXIT, Module}, {reason, Reason}]),
    {stop, {gen_event_EXIT, Module, Reason}, State};
handle_info({'DOWN', _, process, EvMgrRef, Reason}, State) ->
	error_logger:error_report([{gen_event_DOWN, EvMgrRef}, {reason, Reason}]),
	{stop, gen_event_DOWN, State};
handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
