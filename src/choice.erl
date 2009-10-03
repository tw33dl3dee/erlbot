%%%-------------------------------------------------------------------
%%% File    : choice.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 28 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(choice).

-behaviour(gen_server).

%% External exports
-export([start_link/0, start/0, make_choice/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link() -> 
	gen_server:start_link({local, ?MODULE}, choice, [], []).

start() -> 
	gen_server:start({local, ?MODULE}, choice, [], []).

make_choice(Choices) when is_list(Choices), length(Choices) > 1 ->
	gen_server:call(?MODULE, {choose, Choices}).

%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init([]) ->
	random:seed(now()),
	{ok, undefined}.

handle_call({choose, Choices}, _From, State) ->
	{reply, choose(Choices), State};
handle_call(_Request, _From, State) ->
	{reply, nosuchcall, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

choose(Choices) ->
	Sum = weight_sum(Choices),
	choose(Choices, Sum).

choose(Choices, Sum) when Sum > 0 ->
	choose(Choices, 0, random:uniform(Sum) - 1);
choose(_, _) ->
	undefined.

choose([{W, _} | _] = Choices, Quantile, X) when is_integer(W) ->
	choose2(Choices, Quantile, X);
choose([C | Rest], Quantile, X) ->
	choose2([{1, C} | Rest], Quantile, X).

choose2([{W, C} | _], Quantile, X) when X < W + Quantile ->
	C;
choose2([{_, C}], _, _) ->
	C;
choose2([{W, _} | Rest], Quantile, X)->
	choose(Rest, W + Quantile, X).

weight_sum(Choices) ->
	lists:foldl(fun ({W, _}, Sum) when is_integer(W) -> W + Sum; (_, S) -> S + 1 end, 0, Choices).
