%%%-------------------------------------------------------------------
%%% File    : erlbot_ev.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_ev).

-export([start_link/0, notify/1, delayed_notify/2]).

%%% API

start_link() ->
	gen_event:start_link({local, ?MODULE}).

notify(Event) ->
	gen_event:notify(?MODULE, Event).

delayed_notify(Event, Delay) ->
	timer:apply_after(Delay, gen_event, notify, [?MODULE, Event]).
