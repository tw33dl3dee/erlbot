%%%-------------------------------------------------------------------
%%% File    : erlbot_ev.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_ev).

-export([start_link/0]).

%%% API

start_link() ->
	gen_event:start_link({local, ?MODULE}).

