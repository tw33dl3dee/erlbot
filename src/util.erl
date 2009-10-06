-module(util).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-export([split/1, set_flag/2, unset_flag/2]).

split(String) ->
	re:split(String, "\s+", [{return, list}, trim]).

set_flag(Flag, Flags) ->
	[Flag | unset_flag(Flag, Flags)].

unset_flag(Flag, Flags) ->
	lists:delete(Flag, Flags).
