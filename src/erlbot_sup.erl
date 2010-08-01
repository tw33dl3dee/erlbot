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
-export([add_behaviour/1, add_behaviour/2, remove_behaviour/1]).
-export([get_behaviours/0]).
-export([config_change/3]).

%%% supervisor callbacks
-export([init/1]).

-define(CHILD_SHUTDOWN, 60000).  % grace time for children shutdown
-define(MAX_R, 5).            % max restart count
-define(MAX_T, 1).            % ... per this many seconds

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type),     {Mod, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, [Mod]}).
-define(CHILD_DYN(Mod, Args, Type), {Mod, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, dynamic}).

-define(CHILD_BHV(Mod, Args),{Mod, {erlbot_behaviour, start_link, [erlbot_ev, Mod, Args]},
							  permanent, ?CHILD_SHUTDOWN, worker, erlbot_behaviour:modules(Mod)}).

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
%%%   --+ erlbot_sup(ev)
%%%   | |
%%%   | --- erlbot_ev
%%%   | |
%%%   | --- event_sup(BhvName1)
%%%   | |
%%%   | --- event_sup(BhvName2)
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

%% Level = [top | ev]
start_link(Level) ->
	Name = list_to_atom("erlbot_sup_" ++ atom_to_list(Level)),
	supervisor:start_link({local, Name}, ?MODULE, Level).

add_behaviour(BhvMod) ->
	add_behaviour(BhvMod, behaviour_config(BhvMod)).

add_behaviour(BhvMod, BhvConfig) ->
	Res = supervisor:start_child(erlbot_sup_ev, ?CHILD_BHV(BhvMod, BhvConfig)),
	error_logger:info_report([behaviour_added, {module, BhvMod}, {arg, BhvConfig}]),
	Res.

remove_behaviour(BhvMod) ->
	supervisor:terminate_child(erlbot_sup_ev, BhvMod),
	Res = supervisor:delete_child(erlbot_sup_ev, BhvMod),
	error_logger:info_report([behaviour_removed, {module, BhvMod}]),
	Res.

get_behaviours() ->
	[Id || {Id, _, _, _} <- supervisor:which_children(erlbot_sup_ev), Id =/= erlbot_ev].

%% Handles internal config change 
%% (not app(4) config, which is handled by erlbot:config_change/3)
config_change(Changed, New, Removed) ->
	% Reload behaviour list
	case erlbot_config:find_change(behaviours, Changed, New, Removed) of 
		{changed, OldBhv, NewBhv} -> reload_behaviours(OldBhv, NewBhv);
		{new, NewBhv}             -> reload_behaviours([], NewBhv);
		{removed, OldBhv}         -> reload_behaviours(OldBhv, []);
		false                     -> ok
	end,
	% Reload behaviour configs
	ok = erlbot_ev:notify({config_change, Changed, New, Removed}).

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

init(top) ->
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD(throttle, [], worker), 
										  ?CHILD(choice, [], worker),
										  ?CHILD(erlbot_config, [], worker),
										  ?CHILD(erlbot_db, [], worker),
										  ?CHILD(erlbot_sup, [ev], supervisor),
										  ?CHILD(irc_conn, [[connect]], worker)]}};
init(ev) ->
	Behaviours = erlbot_config:get_value(behaviours, []),
	BhvChildren = [?CHILD_BHV(Mod, behaviour_config(Mod)) || Mod <- Behaviours],
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD_DYN(erlbot_ev, [], worker) | BhvChildren]}}.

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

reload_behaviours(OldBhv, NewBhv) -> 
	[remove_behaviour(Bhv) || Bhv <- OldBhv -- NewBhv],
	[add_behaviour(Bhv)    || Bhv <- NewBhv -- OldBhv],
	ok.

behaviour_config(Mod) ->
	erlbot_config:get_value(Mod).
