%%%-------------------------------------------------------------------
%%% File    : erlbot_handler.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 26 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_handler).

-behaviour(gen_event).

%% External exports
-export([add_handler/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-record(state, {}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
add_handler(EventMgr, IrcRef) ->
	event_sup:start_link(EventMgr, undef, ?MODULE, IrcRef).

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%--------------------------------------------------------------------
init(IrcRef) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_event({Type, Event}, State) ->
	io:format("*** ~p: ~p~n", [Type, Event]),
	{ok, State}.

%%--------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
	{ok, nosuchcall, State}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%% Local Variables:
%%% compile-command: "erlc erlbot_handler.erl"
%%% End:
