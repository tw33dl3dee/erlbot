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
-export([start_link/0, start/0, make/1, uniform/0, uniform/1, uniform/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link() -> 
	gen_server:start_link({local, ?MODULE}, choice, [], []).

start() -> 
	gen_server:start({local, ?MODULE}, choice, [], []).

make(Choices) when is_list(Choices), length(Choices) > 1 ->
	gen_server:call(?MODULE, {make_choice, Choices});
make([{_, OnlyChoice}]) -> OnlyChoice;
make([OnlyChoice]) -> OnlyChoice.

uniform() ->
	gen_server:call(?MODULE, uniform).

uniform(N) ->
	gen_server:call(?MODULE, {uniform, N}).

uniform(Min, Max) ->
	uniform(Max - Min + 1) + Min - 1.

%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init([]) ->
	random:seed(now()),
	{ok, undefined}.

handle_call({make_choice, Choices}, _From, State) ->
	Sum = weight_sum(Choices),
	{reply, make_choice(Choices, Sum), State};
handle_call({uniform, N}, _From, State) ->
	{reply, random:uniform(N), State};
handle_call(uniform, _From, State) ->
	{reply, random:uniform(), State};
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

make_choice(Choices, Sum) when Sum > 0 -> make_choice(Choices, 0, random:uniform(Sum) - 1);
make_choice(_, _)                      -> undefined.

make_choice([{W, _} | _] = Choices, Quantile, X) when is_integer(W) ->
	pick_choice(Choices, Quantile, X);
make_choice([C | Rest], Quantile, X) ->
	pick_choice([{1, C} | Rest], Quantile, X).

pick_choice([{W, C} | _], Quantile, X) 
  when X < W + Quantile                   -> C;
pick_choice([{_, C}], _, _)               -> C;
pick_choice([{W, _} | Rest], Quantile, X) -> make_choice(Rest, W + Quantile, X).

weight_sum(Choices) ->
	lists:foldl(fun ({W, _}, Sum) when is_integer(W) -> W + Sum; (_, S) -> S + 1 end, 0, Choices).
