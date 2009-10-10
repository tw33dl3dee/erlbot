%%%-------------------------------------------------------------------
%%% File    : throttle.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 8 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(throttle).

-behaviour(gen_server).

%% External exports
-export([start_link/0, wait/3, wait/4, cancel/1, test/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(trace, {id      :: term(),           %% Throttling id
				timer   :: reference(),      %% timer that fires after period expiration
				times   :: [integer()]}).    %% Last throttle times in milliseconds since Epoch.

-record(state, {traces = []  :: [#trace{}]}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

wait(Id, MaxCount, Period) ->
	wait(Id, MaxCount, Period, fun (D) -> D end).

wait(Id, MaxCount, Period, PreWait) ->
	case gen_server:call(?MODULE, {wait, Id, MaxCount, Period}, infinity) of
		ok ->
			io:format("~p: nowait~n", [Id]),
			{ok, nowait};
		{wait, Delay} ->
			io:format("~p: waiting ~p~n", [Id, Delay]),
			timer:sleep(PreWait(Delay)),
			{ok, waited, Delay}
	end.	

cancel(Id) ->
	gen_server:call(?MODULE, {cancel, Id}, infinity).

test(1) ->
	start_link(),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	timer:sleep(5000);
test(2) ->
	start_link(),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	timer:sleep(500),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	timer:sleep(1000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	wait(abc, 3, 5000),
	wait(def, 3, 5000),
	timer:sleep(5000);
test(3) ->
	start_link(),
	Me = self(),
	spawn(fun () ->
				  wait(abc, 3, 5000),
				  wait(abc, 3, 5000),
				  timer:sleep(500),
				  wait(abc, 3, 5000),
				  timer:sleep(1000),
				  wait(abc, 3, 5000),
				  wait(abc, 3, 5000),
				  wait(abc, 3, 5000),
				  Me ! done
		  end),
	spawn(fun () ->
				  wait(def, 3, 5000),
				  wait(def, 3, 5000),
				  wait(def, 3, 5000),
				  cancel(def),
				  wait(def, 3, 5000),
				  wait(def, 3, 5000),
				  wait(def, 3, 5000),
				  wait(def, 3, 5000, fun (D) -> io:format("waiting ~p msec~n", [D]), D end),
				  wait(def, 3, 5000),
				  Me ! done
		  end),
	receive done -> receive done -> ok end end,
	timer:sleep(5000).

%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init(_) ->
	{ok, #state{}}.

-define(EXPIRE_DELAY, 1000).

handle_call({wait, Id, MaxCount, Period}, _From, #state{traces = Traces} = State) ->
	TimerRef = erlang:start_timer(Period + ?EXPIRE_DELAY, self(), Id),
	case lists:keytake(Id, #trace.id, Traces) of
		{value, Trace, Rest} ->
			{Reply, NewTrace} = filter_trace(set_timer(Trace, TimerRef), MaxCount, Period),
			{reply, Reply, State#state{traces = [NewTrace | Rest]}};
		false ->
			NewTrace = add_trace(Id, TimerRef),
			{reply, ok, State#state{traces = [NewTrace | Traces]}}
	end;
handle_call({cancel, Id}, _From, State) ->
	case remove_trace(Id, State) of 
		{Reply, Trace, NewState} -> 
			set_timer(Trace, undefined),
			{reply, Reply, NewState} 
	end.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({timeout, _, Id}, State) ->
	io:format("Removing trace ~p~n", [Id]),
	case remove_trace(Id, State) of {ok, _, NewState} -> {noreply, NewState} end.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

add_trace(Id, TimerRef) ->
	#trace{id = Id, timer = TimerRef, times = [util:epoch(msec)]}.

filter_trace(Trace, MaxCount, Period) ->
	Now = util:epoch(msec),
	Times = lists:filter(fun (T) when Now - T > Period -> false;
							 (_)                       -> true end,
						 [Now | Trace#trace.times]),
	case length(Times) of
		L when L > MaxCount ->
			TimeDiff = Now - lists:nth(MaxCount + 1, Times),
			{{wait, Period - TimeDiff}, Trace#trace{times = Times}};
		_ ->
			{ok, Trace#trace{times = Times}}
	end.

remove_trace(Id, State) ->
	case lists:keytake(Id, #trace.id, State#state.traces) of 
		{value, Trace, Rest} ->
			{ok, Trace, State#state{traces = Rest}};
		false ->
			{not_found, undefined, State}
	end.

set_timer(#trace{timer = OldTimer} = Trace, Timer) ->
	case erlang:cancel_timer(OldTimer) of
		cancel ->
			clean_msg(OldTimer);
		T ->
			T
	end,
	Trace#trace{timer = Timer}.

clean_msg(TimerRef) ->
	receive 
		{timeout, TimerRef, _} ->
			ok
	after 0 ->
			ok
	end.
