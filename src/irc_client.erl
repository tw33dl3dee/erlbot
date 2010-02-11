%%%-------------------------------------------------------------------
%%% File    : irc_client.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 24 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(irc_client).

-behaviour(supervisor).

%% External API
-export([start_link/2, start_link/3, add_behaviour/2, remove_behaviour/2, irc_conn/1, run/0, reload/0]).

%% Internal entry points
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% Testing interface
-export([test/0]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(ClientName, ConnArgs, Behaviours) ->
	{ok, SupPid} = supervisor:start_link(ClientName, ?MODULE, top),
	start_irc(SupPid, ConnArgs, Behaviours).

start_link(ConnArgs, Behaviours) ->
	{ok, SupPid} = supervisor:start_link(?MODULE, top),
	start_irc(SupPid, ConnArgs, Behaviours).

%% Currently unused.
start_link(Level) ->
	supervisor:start_link(?MODULE, Level).

add_behaviour(SupRef, BhvMod) ->
	{ok, _} = supervisor:start_child(SupRef, bhv_spec(SupRef, BhvMod)).

remove_behaviour(SupRef, BhvMod) ->
	ok = supervisor:terminate_child(SupRef, BhvMod),
	supervisor:delete_child(SupRef, BhvMod).

irc_conn(SupRef) ->
	whereis_child(SupRef, irc_conn).

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

-define(CHILD_SHUTDOWN, 60000).
-define(MAX_R, 1000).
-define(MAX_T, 1).

init(_) ->
	Throttle = {throttle, {throttle, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, [throttle]},
	Choice = {choice, {choice, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, [choice]},
	EvMgr = {ev_mgr, {gen_event, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, dynamic},
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [Choice, Throttle, EvMgr]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

start_irc(SupPid, ConnArgs, Behaviours) ->
	[add_behaviour(SupPid, BhvMod) || BhvMod <- Behaviours],
	Notifier = fun (Type, Event, Irc) -> 
					   case ev_mgr(SupPid) of 
						   undefined ->
							   {error, badarg};
						   Pid ->
							   gen_event:notify(Pid, {Type, Event, Irc})
					   end
			   end,
	Irc = {irc_conn, {irc_conn, start_link, [{local, irc}, Notifier, ConnArgs]}, permanent, ?CHILD_SHUTDOWN, worker, 
		   [irc_conn, irc_proto, irc_chan, irc_codes]},
	{ok, _} = supervisor:start_child(SupPid, Irc),
	{ok, SupPid}.

bhv_spec(SupRef, BhvMod) ->
	{BhvMod, {irc_behaviour, add_behaviour, [ev_mgr(SupRef), BhvMod]}, permanent, ?CHILD_SHUTDOWN, worker, irc_behaviour:modules(BhvMod)}.

whereis_child(SupRef, ChildId) ->
	ChildrenInfo = supervisor:which_children(SupRef),
	whereis_child(ChildrenInfo, ChildId, []).

whereis_child([{ChildId, Pid, _, _} | _], ChildId, _) ->
	Pid;
whereis_child([{_, Pid, supervisor, _} | Rest], ChildId, SubSups) -> 
	whereis_child(Rest, ChildId, SubSups ++ [Pid]);
whereis_child([_ | Rest], ChildId, SubSups) ->
	whereis_child(Rest, ChildId, SubSups);
whereis_child([], ChildId, [Pid | SubSups]) ->
	whereis_child(supervisor:which_children(Pid), ChildId, SubSups);
whereis_child([], _, []) ->
	undefined.

ev_mgr(SupRef) ->
	whereis_child(SupRef, ev_mgr).

%%%-------------------------------------------------------------------
%%% Conventional API (to be replaced with OTP application)
%%%-------------------------------------------------------------------

-include(".secret.hrl").

-define(BEHAVIOURS, [bhv_err_print, bhv_log, bhv_appeal, bhv_chancmd, bhv_getop, bhv_pom, bhv_privcmd, bhv_comment, bhv_bash,
					 bhv_google, bhv_help, bhv_lebedev, bhv_lojban, bhv_lurkmore, bhv_misc, bhv_wiki, bhv_blurp, bhv_giveop, bhv_greet, 
					 bhv_rejoin, bhv_suicide, bhv_history, bhv_stat, bhv_xlit]).

run() ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"localhost", "nya", [{login, "nya"}, {autojoin, ["#pron", "#work", "#mstu"]}, {impl_umode, "+F"}]}, 
						   ?BEHAVIOURS),
	unlink(Pid),
	Pid.

%% Reload all modules under current working dir.
reload() ->
	{ok, Pwd} = file:get_cwd(),
	[begin io:format("Reloading ~p...~n", [Mod]),
		   true          = code:soft_purge(Mod),
		   {module, Mod} = code:load_file(Mod),
		   Mod
	 end || {Mod, ModPath} <- code:all_loaded(), is_list(ModPath), string:str(ModPath, Pwd) == 1].

%%%-------------------------------------------------------------------
%%% Testing
%%%-------------------------------------------------------------------

test() ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"localhost", "nya", [{login, "nya"}, {autojoin, ["#test"]}, {impl_umode, "+F"}]}, 
						   ?BEHAVIOURS),
	unlink(Pid),
	Pid.
