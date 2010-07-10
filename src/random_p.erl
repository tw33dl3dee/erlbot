%%% Solution for http://www.lshift.net/blog/2006/09/06/random-in-erlang
%%%
%%% 2006-09-13  Stefan Scholl <stesch@no-spoon.de>
%%% 2008-12-29  Ivan Korotkov <twee@tweedle-dee.org>
%%%
%%% Public Domain (sic!)
%%%

-module(random_p).
-export([start/0, stop/0, uniform/0, uniform/1, generator/0]).
-author('stesch@no-spoon.de').
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-define(NAME, random_p_generator).

%% Start the random-generator process
start() ->
	case whereis(?NAME) of
		undefined ->
			Pid = spawn_link(?MODULE, generator, []),
			register(?NAME, Pid),
			?NAME ! seed;
		_ ->
			ok
	end,
	true.

%% Process shutdown
stop() ->
	?NAME ! shutdown,
	true.

%% The actual process with the random state
generator() ->
	receive
		seed ->
			{A1, A2, A3} = now(),
			random:seed(A1, A2, A3),
			generator();
		shutdown ->
			true;
		{uniform, Pid} ->
			Pid ! {?NAME, random:uniform()},
			generator();
		{uniform, Pid, N} ->
			Pid ! {?NAME, N, random:uniform(N)},
			generator()
	end.

%% Getting the next uniform random number
uniform() ->
	?NAME ! {uniform, self()},
	receive {?NAME, Random} -> Random end.

uniform(N) ->
	?NAME ! {uniform, self(), N},
	receive {?NAME, N, Random} -> Random end.
