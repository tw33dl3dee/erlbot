%%%-------------------------------------------------------------------
%%% File    : erlbot_sup.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 24 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1, start_link/2]).

%% supervisor callbacks
-export([init/1]).

-export([test/0]).
-include(".secret.hrl").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Args) ->
	start_link(top, Args).

start_link(top, Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, {top, Args});
start_link(sub, Args) ->
	supervisor:start_link(?MODULE, {sub, Args}).

test() ->
	application:start(proc),
	start_link({{local, irc}, "192.168.1.1", "test", [{login, "nya"}, {oper_pass, ?MAGIC_WORD}, {autojoin, ["#test", "#pron", "#mstu"]}]}).

%% start_proc_super() ->
%% 	case proc_super:start_link(top) of
%% 		{error, {already_started, _}} ->
%% 			ignore;
%% 		Result ->
%% 			Result
%% 	end.

%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init({top, {Name, Host, Nick, Options}}) ->
	{IrcRef, EvMgrRef} = {make_ref(), make_ref()},	
	Notifier = fun (Type, Event) -> 
					   case proc:where(EvMgrRef) of 
						   undefined ->
							   {error, badarg};
						   Pid ->
							   gen_event:notify(Pid, {Type, Event})
					   end
			   end,
	%%Proc = {"Process Registry", {?MODULE, start_proc_super, []}, permanent, infinity, supervisor, [proc_super]},
	Irc = {"IRC protocol", {irc_conn, start_link, [Name, Host, Nick, Notifier, Options]}, permanent, 60000, worker, [irc_conn, gen_irc, irc_chan, irc_codes]},
	HandlerSup = {"IRC events handlers supervisor", {?MODULE, start_link, [sub, {EvMgrRef, IrcRef}]}, permanent, infinity, supervisor, [?MODULE]},
	{ok, {{one_for_one, 10, 5}, [Irc, HandlerSup]}};
init({sub, {EvMgrRef, IrcRef}}) ->
	EvMgr = {"IRC events handler manager", {gen_event_proc, start_link, [EvMgrRef]}, permanent, 10000, worker, dynamic},
	Erlbot = {"Erlbot handler", {erlbot_handler, add_handler, [EvMgrRef, IrcRef]}, permanent, 10000, worker, [erlbot_handler]},
	{ok, {{one_for_all, 10, 5}, [EvMgr, Erlbot]}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%%% Local Variables:
%%% compile-command: "erlc erlbot_sup.erl"
%%% End:
