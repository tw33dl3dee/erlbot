%%%-------------------------------------------------------------------
%%% File    : irc_client.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 24 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(irc_client).

-behaviour(supervisor).

%% External exports
-export([start_link/1, start_link/2, start/1, add_behaviour/2, irc_conn/1]).

%% supervisor callbacks
-export([init/1]).

-export([test/0, whereis_child/2, test_whereis/1]).
-include(".secret.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start(Args) ->
	{ok, Pid} = start_link(Args),
	unlink(Pid),
	{ok, Pid}.

start_link(Args) ->
	start_link(top, Args).

start_link(top, Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {top, Args});
start_link(Level, Args) ->
	supervisor:start_link(?MODULE, {Level, Args}).

irc_conn(SupRef) ->
	whereis_child(SupRef, irc_conn).

add_behaviour(SupRef, BhvMod) ->
	io:format("add bhv ~p to ~p~n", [BhvMod, SupRef]),
	supervisor:start_child(SupRef, bhv_spec(SupRef, BhvMod)).

test() ->
	application:start(proc),
	{ok, Pid} = start_link({{local, irc}, "192.168.1.1", "yest", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#test", "#mstu"]}, {umode, "+F"}], [bhv1]}).
	%add_behaviour(Pid, bhv1),
	%add_behaviour(Pid, bhv2),
	%add_behaviour(Pid, bhv3),
	%add_behaviour(Pid, bhv4).

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

-define(CHILD_SHUTDOWN, 60000).
-define(MAX_R, 10).
-define(MAX_T, 10).

init({top, {Name, Host, Nick, Options, Behaviours}}) ->
	SupRef = self(),
	Notifier = fun (Type, Event) -> 
					   case ev_mgr(SupRef) of 
						   undefined ->
							   {error, badarg};
						   Pid ->
							   ConnRef = irc_conn(SupRef),
							   gen_event:notify(Pid, {Type, Event, ConnRef})
					   end
			   end,
	Irc = {irc_conn, {irc_conn, start_link, [Name, Host, Nick, Notifier, Options]}, permanent, ?CHILD_SHUTDOWN, worker, [irc_conn, irc_proto, irc_chan, irc_codes]},
	HandlerSup = {handlers_sup, {?MODULE, start_link, [handlers, {Behaviours, SupRef}]}, permanent, infinity, supervisor, [?MODULE]},
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, [Irc, HandlerSup]}};
init({handlers, {Behaviours, SupRef}}) ->
	EvMgr = {ev_mgr, {gen_event, start_link, []}, permanent, ?CHILD_SHUTDOWN, worker, dynamic},
	BhvSup = {bhv_sup, {?MODULE, start_link, [bhv, {Behaviours, SupRef}]}, permanent, infinity, supervisor, [?MODULE]},
	{ok, {{rest_for_one, ?MAX_R, ?MAX_T}, [EvMgr, BhvSup]}};
init({bhv, {Behaviours, SupRef}}) ->
	Bhvs = lists:map(fun (BhvMod) -> bhv_spec(SupRef, BhvMod) end, Behaviours),
	{ok, {{one_for_one, ?MAX_R, ?MAX_T}, []}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

test_whereis(SupRef) ->
	io:format("then: ~p~n", [now()]),
	lists:map(fun (_) -> whereis_child(SupRef, handler) end, lists:seq(1, 1000000)),
	io:format("later: ~p~n", [now()]),
	lists:map(fun (_) -> whereis(?MODULE) end, lists:seq(1, 1000000)),
	io:format("now: ~p~n", [now()]).	

whereis_child(SupRef, ChildId) ->
	io:format("==whereis_child, self ~p, super ~p==~n", [self(), SupRef]),
	ChildrenInfo = supervisor:which_children(SupRef),
	io:format("search ~p in ~p~n", [ChildId, ChildrenInfo]),
	whereis_child(ChildrenInfo, ChildId, []).

whereis_child([{ChildId, Pid, _, _} | _], ChildId, _) ->
%	io:format("found ~p~n", [Pid]),
	Pid;
whereis_child([{_, Pid, supervisor, _} | Rest], ChildId, SubSups) -> 
%	io:format("add supervisor ~p to ~p~n", [Pid, SubSups]),
	whereis_child(Rest, ChildId, SubSups ++ [Pid]);
whereis_child([_ | Rest], ChildId, SubSups) ->
%	io:format("looking for ~p in ~p~n", [ChildId, Rest]),
	whereis_child(Rest, ChildId, SubSups);
whereis_child([], ChildId, [Pid | SubSups]) ->
%	io:format("descend to supervisor ~p~n", [Pid]),
	whereis_child(supervisor:which_children(Pid), ChildId, SubSups);
whereis_child([], _, []) ->
%	io:format("not found :(~n", []),
	undefined.

bhv_spec(SupRef, BhvMod) ->
	{BhvMod, {irc_behaviour, add_behaviour, [ev_mgr(SupRef), BhvMod]}, permanent, ?CHILD_SHUTDOWN, worker, irc_behaviour:modules(BhvMod)}.

ev_mgr(SupRef) ->
	io:format("child lookup ev_mgr for ~p~n", [SupRef]),
	whereis_child(SupRef, ev_mgr).
