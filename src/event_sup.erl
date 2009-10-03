-module(event_sup).
-behaviour(gen_server).

-export([start_link/4]).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link(EventMgr, undef, Module, Args) ->
    gen_server:start_link(?MODULE, {EventMgr, Module, Args}, []);
start_link(EventMgr, Name, Module, Args) ->
    gen_server:start_link(Name, ?MODULE, {EventMgr, Module, Args}, []).

init({EventMgr, Module, Args}) ->
    ok = gen_event:add_sup_handler(EventMgr, Module, Args),
    {ok, {EventMgr, Module}}.

handle_call(_Req, _From, S) ->
    {stop, unknown_call, S}.

handle_cast(_From, S) ->
    {stop, unknown_cast, S}.

handle_info({gen_event_EXIT, Mod, Reason}, State) ->
    {stop, {gen_event_EXIT, Mod, Reason}, State}.

terminate(_Reason, {_EventMgr, _Module}) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
