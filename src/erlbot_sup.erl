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
-export([add_behaviour/1, remove_behaviour/1]).

%%% supervisor callbacks
-export([init/1]).

-define(CHILD_SHUTDOWN, 60000).  % grace time for children shutdown
-define(MAX_R, 1000).            % max restart count
-define(MAX_T, 1).               % ... per this many seconds

%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args, Type),     {Mod, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, [Mod]}).
-define(CHILD_DYN(Mod, Args, Type), {Mod, {Mod, start_link, Args}, permanent, ?CHILD_SHUTDOWN, Type, dynamic}).

-define(CHILD_BHV(Mod),{Mod, {erlbot_behaviour, start_link, [erlbot_ev, Mod, undefined]},
						permanent, ?CHILD_SHUTDOWN, worker, erlbot_behaviour:modules(Mod)}).

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
%%%   --+ irc_conn
%%%   | |
%%%   | --- irc_proto
%%%   | |
%%%   | --- irc_chan(Chan1)
%%%   | |
%%%   | --- irc_chan(Chan2)
%%%   | |
%%%   | ...
%%%   |
%%%   --+ erlbot_sup(ev)
%%%     |
%%%     --- erlbot_ev
%%%     |
%%%     --- event_sup(BhvName1)
%%%     |
%%%     --- event_sup(BhvName2)
%%%     |
%%%     ...

%% Level = [top | ev]
start_link(Level) ->
	Name = list_to_atom("erlbot_sup_" ++ atom_to_list(Level)),
	supervisor:start_link({local, Name}, ?MODULE, Level).

add_behaviour(BhvMod) ->
	{ok, _} = supervisor:start_child(erlbot_sup_ev, ?CHILD_BHV(BhvMod)).

remove_behaviour(BhvMod) ->
	ok = supervisor:terminate_child(erlbot_sup_ev, BhvMod),
	supervisor:delete_child(erlbot_sup_ev, BhvMod).

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

init(top) ->
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD(throttle, [], worker), 
										  ?CHILD(choice, [], worker),
										  ?CHILD(irc_conn, [], worker),
										  ?CHILD(erlbot_sup, [ev], supervisor)]}};
init(ev) ->
	{ok, Behaviours} = application:get_env(behaviours),
	BhvChildren = [?CHILD_BHV(BhvName) || BhvName <- Behaviours],
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [?CHILD_DYN(erlbot_ev, [], worker) | BhvChildren]}}.
