%%%-------------------------------------------------------------------
%%% File    : gen_event_proc.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 26 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(gen_event_proc).

-behaviour(gen_event).

%% External exports
-export([start/1, start_link/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Name) ->
	{ok, Pid} = gen_event:start_link(),
	gen_event:add_handler(Pid, ?MODULE, Name),
	{ok, Pid}.

start(Name) ->
	{ok, Pid} = gen_event:start(),
	gen_event:add_handler(Pid, ?MODULE, Name),
	{ok, Pid}.

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%--------------------------------------------------------------------
init(Name) ->
	proc:reg(Name),
	{ok, undef}.

%%--------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_event(Event, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%--------------------------------------------------------------------
handle_call(Request, State) ->
	{ok, nosuchcall, State}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_info(Info, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%% Local Variables:
%%% compile-command: "erlc gen_event_proc.erl"
%%% End:
