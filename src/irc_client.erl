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
-export([start_link/2, start_link/3, add_behaviour/2, irc_conn/1, run/0, reload/0]).

%% Internal entry points
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% Testing interface
-export([test/0, test/1]).

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

irc_conn(SupRef) ->
	whereis_child(SupRef, irc_conn).

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

-define(CHILD_SHUTDOWN, 60000).
-define(MAX_R, 10).
-define(MAX_T, 10).

init(_) ->
	Throttle = {throttle, {throttle, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, [throttle]},
	Choice = {choice, {choice, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, [choice]},
	EvMgr = {ev_mgr, {gen_event, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, dynamic},
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [Choice, Throttle, EvMgr]}}.

%%%-------------------------------------------------------------------
%%% Conventional API (to be replaced with OTP application)
%%%-------------------------------------------------------------------

-include(".secret.hrl").

run() ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"192.168.1.1", "nya", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#pron", "#work", "#mstu"]}, {umode, "+F"}]}, 
						   [bhv_err_print, bhv_log, bhv_test, bhv_appeal, bhv_chancmd, bhv_getop, bhv_pom, bhv_privcmd, bhv_comment, bhv_bash, 
							bhv_google, bhv_lebedev, bhv_lojban, bhv_lurkmore, bhv_misc, bhv_wiki, bhv_blurp, bhv_giveop, bhv_greet]),
	unlink(Pid),
	Pid.

%% Reload all modules under current working dir.
reload() ->
	{ok, Pwd} = file:get_cwd(),
	[begin code:purge(Mod),
		   code:load_file(Mod) 
	 end || {Mod, ModPath} <- code:all_loaded(), is_list(ModPath), string:str(ModPath, Pwd) == 1].

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

%%% Testing

test() ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"192.168.1.1", "yest", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#test"]}, {umode, "+F"}]}, 
						   [bhv_err_print, bhv_log, bhv_test, bhv_appeal, bhv_chancmd, bhv_getop, bhv_pom, bhv_privcmd, bhv_comment, bhv_bash, 
							bhv_google, bhv_lebedev, bhv_lojban, bhv_lurkmore, bhv_misc, bhv_wiki, bhv_blurp, bhv_giveop, bhv_greet]),
	unlink(Pid),
	Pid.

test(1) ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"192.168.1.1", "yest", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#test", "#mstu"]}, {umode, "+F"}]}, 
						   [bhv_log, bhv1, bhv2, bhv3, bhv4]),
	unlink(Pid),
	timer:sleep(5000),
	irc_conn:quit(irc_client:irc_conn(Pid), "quit"),
	Pid;
test(2) ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"192.168.1.1", "yest", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#test", "#mstu"]}, {umode, "+F"}]}, 
						   [bhv_log, bhv1, bhv2, bhv3, bhv4]),
	unlink(Pid),
	timer:sleep(5000),
	irc_conn:command(irc_client:irc_conn(Pid), nosuchcmd),
	Pid;
test(3) ->
	{ok, Pid} = start_link({local, erlbot}, 
						   {"192.168.1.1", "yest", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#test", "#mstu"]}, {umode, "+F"}]}, 
						   [bhv_log, bhv1, bhv2, bhv3, bhv4]),
	unlink(Pid),
	timer:sleep(100),
	irc_conn:get_channels(irc_client:irc_conn(Pid)),
	Pid.
