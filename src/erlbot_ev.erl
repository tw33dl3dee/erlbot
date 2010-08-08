%%%-------------------------------------------------------------------
%%% File    : erlbot_ev.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_ev).

-export([start_link/1, name/1, config_change/4]).
-export([notify/1, delayed_notify/2, filter/1]).

%%%--------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------

%% Event manager name for specific module type
name(behaviours) -> erlbot_ev_bhv;
name(filters)    -> erlbot_ev_flt.

%% Type = `behaviours' | `filters'
start_link(Type) ->
	gen_event:start_link({local, name(Type)}).

%% Notify all modules about config change
config_change(Type, Changed, New, Removed) ->
	gen_event:notify(name(Type), {config_change, Changed, New, Removed}).

%%% Behaviours stuff

notify(Event) ->
	gen_event:notify(name(behaviours), Event).

delayed_notify(Event, Delay) ->
	timer:apply_after(Delay, gen_event, notify, [name(behaviours), Event]).

%%% Filters stuff

filter(Command) ->
	case filter(Command, gen_event:which_handlers(name(filters))) of
		{error, _} -> Command;
		NewCommand -> NewCommand
	end.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

filter(Command, [Handler | Rest]) ->
	filter(gen_event:call(name(filters), Handler, Command), Rest);
filter(Command, []) -> Command.
