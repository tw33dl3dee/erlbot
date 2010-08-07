%%%-------------------------------------------------------------------
%%% File    : erlbot_sup.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 24 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_sup).

-behaviour(supervisor).

%%% API
-export([start_link/1]).
-export([add_module/2, add_module/3, remove_module/2]).
-export([get_modules/1]).
-export([config_change/3]).

%%% supervisor callbacks
-export([init/1]).

-define(CHILD_SHUTDOWN, 60000).  % grace time for children shutdown
-define(MAX_R, 5).            % max restart count
-define(MAX_T, 1).            % ... per this many seconds

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type),        {Mod, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, [Mod]}).
-define(CHILD2(Name, Mod, Args, Type), {Name, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, [Mod]}).
-define(CHILD_DYN(Mod, Args, Type),    {Mod, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, dynamic}).

-define(CHILD_BHV(Mod, Args),{Mod, {erlbot_behaviour, start_link, [erlbot_ev:name(behaviours), Mod, Args]},
							  permanent, ?CHILD_SHUTDOWN, worker, erlbot_behaviour:modules(Mod)}).

-define(CHILD_FLT(Mod, Args),{Mod, {erlbot_filter, start_link, [erlbot_ev:name(filters), Mod, Args]},
							  permanent, ?CHILD_SHUTDOWN, worker, erlbot_filter:modules(Mod)}).

%%%-------------------------------------------------------------------
%%% Process layout:
%%%
%%% + erlbot_app
%%% |
%%% --+ erlbot_sup(top)
%%%   |
%%%   --- choice
%%%   |
%%%   --- throttle
%%%   |
%%%   --- erlbot_config
%%%   |
%%%   --- couchbeam
%%%   |
%%%   --+ erlbot_sup(behaviours)
%%%   | |
%%%   | --- erlbot_ev(behaviours)
%%%   | |
%%%   | --- event_sup(BhvName1)
%%%   | |
%%%   | --- event_sup(BhvName2)
%%%   | |
%%%   | ...
%%%   |
%%%   --+ erlbot_sup(filters)
%%%   | |
%%%   | --- erlbot_ev(filters)
%%%   | |
%%%   | --- event_sup(FltName1)
%%%   | |
%%%   | --- event_sup(FltName2)
%%%   | |
%%%   | ...
%%%   |
%%%   --+ irc_conn
%%%     |
%%%     --- irc_proto
%%%     |
%%%     --- irc_chan(Chan1)
%%%     |
%%%     --- irc_chan(Chan2)
%%%     |
%%%     ...
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Configuration
%%%
%%% Key           | Description                             | Default 
%%% ------------------------------------------------------------------
%%% behaviours    | list of behaviour modules to load       | []
%%%
%%%-------------------------------------------------------------------

%% Level = [top | behaviours | filters]
start_link(Level) ->
	Name = list_to_atom("erlbot_sup_" ++ atom_to_list(Level)),
	supervisor:start_link({local, Name}, ?MODULE, Level).

add_module(Type, Mod) ->
	add_module(Type, Mod, module_config(Mod)).

add_module(behaviours, Mod, Config) ->
	error_logger:info_report([behaviour_added, {module, Mod}, {arg, Config}]),
	supervisor:start_child(erlbot_sup_behaviours, ?CHILD_BHV(Mod, Config));
add_module(filters, Mod, Config) ->
	error_logger:info_report([filter_added, {module, Mod}, {arg, Config}]),
	supervisor:start_child(erlbot_sup_filters, ?CHILD_FLT(Mod, Config)).

remove_module(behaviours, Mod) ->
	error_logger:info_report([behaviour_removed, {module, Mod}]),
	supervisor:terminate_child(erlbot_sup_behaviours, Mod),
	supervisor:delete_child(erlbot_sup_behaviours, Mod);
remove_module(filters, Mod) ->
	error_logger:info_report([filter_removed, {module, Mod}]),
	supervisor:terminate_child(erlbot_sup_filters, Mod),
	supervisor:delete_child(erlbot_sup_filters, Mod).

get_modules(behaviours) ->
	[Id || {Id, _, _, _} <- supervisor:which_children(erlbot_sup_behaviours), Id =/= erlbot_ev];
get_modules(filters) ->
	[Id || {Id, _, _, _} <- supervisor:which_children(erlbot_sup_filters), Id =/= erlbot_ev].

%% Handles internal config change 
%% (not app(4) config, which is handled by erlbot:config_change/3)
config_change(Changed, New, Removed) ->
	config_change(behaviours, Changed, New, Removed),
	config_change(filters, Changed, New, Removed).

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

init(top) ->
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD(throttle, [], worker), 
										  ?CHILD(choice, [], worker),
										  ?CHILD(erlbot_config, [], worker),
										  ?CHILD(erlbot_db, [], worker),
										  ?CHILD2(erlbot_sup1, erlbot_sup, [behaviours], supervisor),
										  ?CHILD2(erlbot_sup2, erlbot_sup, [filters], supervisor),
										  ?CHILD(irc_conn, [[connect]], worker)]}};
init(behaviours) ->
	Behaviours = erlbot_config:get_value(behaviours, []),
	BhvChildren = [?CHILD_BHV(Mod, module_config(Mod)) || Mod <- Behaviours],
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD_DYN(erlbot_ev, [behaviours], worker) | BhvChildren]}};
init(filters) ->
	Filters = erlbot_config:get_value(filters, []),
	FltChildren = [?CHILD_FLT(Mod, module_config(Mod)) || Mod <- Filters],
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD_DYN(erlbot_ev, [filters], worker) | FltChildren]}}.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

config_change(Type, Changed, New, Removed) ->
	% Reload module list
	case erlbot_config:find_change(Type, Changed, New, Removed) of 
		{changed, OldBhv, NewBhv} -> reload_modules(Type, OldBhv, NewBhv);
		{new, NewBhv}             -> reload_modules(Type, [], NewBhv);
		{removed, OldBhv}         -> reload_modules(Type, OldBhv, []);
		false                     -> ok
	end,
	% Reload module configs
	erlbot_ev:config_change(Type, Changed, New, Removed).

reload_modules(Type, Old, New) -> 
	[remove_module(Type, Mod) || Mod <- Old -- New],
	[add_module(Type, Mod)    || Mod <- New -- Old],
	ok.

module_config(Mod) ->
	erlbot_config:get_value(Mod).
