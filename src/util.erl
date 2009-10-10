-module(util).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-export([split/1, set_flag/2, unset_flag/2, epoch/0, epoch/1]).

split(String) ->
	re:split(String, "\s+", [{return, list}, trim]).

set_flag(Flag, Flags) ->
	[Flag | unset_flag(Flag, Flags)].

unset_flag(Flag, Flags) ->
	lists:delete(Flag, Flags).

epoch() -> epoch(sec).

epoch(sec) ->
	case erlang:now() of {M, S, _} -> M*1000000 + S end;
epoch(msec) ->
	case erlang:now() of {M, S, U} -> (M*1000000 + S)*1000 + U div 1000 end;
epoch(usec) ->
	case erlang:now() of {M, S, U} -> (M*1000000 + S)*1000000 + U end.
