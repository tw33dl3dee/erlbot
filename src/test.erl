-module(test).

%% -export([myfunc/1]).

%% -compile({parse_transform, test_pt}).

%% -define(in, :).

%% myfunc(X) when is_atom(X), X : {a, b, c} ->
%% 	case X of 
%% 		X when X <+ {d, e, f} ->
%% 			ok
%% 	end,
%% 	if X -- {g, h, i} ->
%% 			ok
%% 	end,
%% 	try X of
%% 		X when X ?in {j, k, l} ->
%% 			ok
%% 	catch
%% 		exit:Y when Y !- {m, n, o} ->
%% 			ok
%% 	end,
%% 	receive 
%% 		X when X =- {p, q, r} ->
%% 			ok
%% 	end.

%% myfunc2(X) when is_atom(X), (X =:= a orelse X =:= b orelse X =:= c) ->
%% 	ok.
