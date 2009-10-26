-module(irc_conn).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_fsm
-behaviour(gen_fsm).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).
-export([state_connecting/2, state_auth_nick/2, state_auth_oper/2, state_auth_end/2, state_connected/2, state_connected/3]).

%% public interface
-export([start/3, start_link/3,start/2, start_link/2]).
-export([chanmsg/3, privmsg/3, join/2, part/2, quit/2, action/3, mode/4, umode/2, kick/4, topic/3, nick/2, command/2]).
-export([get_channels_info/1, get_channels/1, get_channel_info/2]).
-export([each_channel/2, each_channel/3, is_user_present/3]).
-export([async_chanmsg/3, async_action/3, async_privmsg/3]).

-record(conf, {nick      = [] :: list(),      %% initial nick requested
			   login     = [] :: list(),      %% login field in USER and OPER commands (defaults to nick)
			   long_name = [] :: list(),      %% long name in USER command (defaults to nick)
			   oper_pass = [] :: list(),      %% password in OPER command (won't do OPER if not specified)
			   umode     = [] :: list(),      %% umode spec to request initially (like, "+F")
			   autojoin  = [] :: [list()],    %% channels to join automatically
			   msg_interval = 200,            %% minimal interval between messages when sending long bulks
			   conn_rate    = {2, 16000}}).   %% maximum connection rate

-record(conn, {nick                   :: list(),    %% actual nick (may differ from initially requested one)
			   nick_suffix = 1        :: integer(), %% suffix appended to nick on nick collision
			   login                  :: list(),    %% actual login used
			   is_oper   = false      :: boolean(),
			   conf                   :: #conf{},
			   irc_ref                :: pid(),
			   handler                :: function(),
			   chan_fsms = dict:new() :: dict()}).  %% channame -> pid()

-include("irc.hrl").

%% public interface

start(Handler, {Host, Nick, Options}) ->
	gen_fsm:start(?MODULE, {Host, Nick, Handler, Options}, []).
start(FsmName, Handler, {Host, Nick, Options}) ->
	gen_fsm:start(FsmName, ?MODULE, {Host, Nick, Handler, Options}, []).

start_link(Handler, {Host, Nick, Options}) ->
	gen_fsm:start_link(?MODULE, {Host, Nick, Handler, Options}, []).
start_link(FsmName, Handler, {Host, Nick, Options}) ->
	gen_fsm:start_link(FsmName, ?MODULE, {Host, Nick, Handler, Options}, []).

%%% IRC commands
%%% Irc may be #irc{} or FSM pid/name

sync_command(#irc{conn_ref = FsmRef}, Cmd) ->
	gen_fsm:sync_send_event(FsmRef, Cmd, infinity);
sync_command(FsmRef, Cmd) when is_pid(FsmRef); is_atom(FsmRef) ->
	gen_fsm:sync_send_event(FsmRef, Cmd, infinity).

async_command(#irc{conn_ref = FsmRef}, Cmd) ->
	gen_fsm:send_event(FsmRef, Cmd);
async_command(FsmRef, Cmd) when is_pid(FsmRef); is_atom(FsmRef) ->
	gen_fsm:send_event(FsmRef, Cmd).

command(Irc, Cmd) ->
	async_command(Irc, {irc_command, Cmd}).

chanmsg(Irc, Channel, Msg) ->
	command(Irc, {chanmsg, Channel, Msg}).

privmsg(Irc, To, Msg) ->
	command(Irc, {privmsg, To, Msg}).

action(Irc, Channel, Action) ->
	command(Irc, {action, Channel, Action}).

join(Irc, Channel) ->
	command(Irc, {join, Channel}).

part(Irc, Channel) ->
	command(Irc, {part, Channel}).

quit(Irc, QuitMsg) ->
	command(Irc, {quit, QuitMsg}).

mode(Irc, Channel, User, Mode) ->
	command(Irc, {mode, Channel, User, Mode}).

umode(Irc, Mode) ->
	command(Irc, {umode, Mode}).

nick(Irc, Nick) ->
	command(Irc, {nick, Nick}).

kick(Irc, Channel, Nick, Reason) ->
	command(Irc, {kick, Channel, Nick, Reason}).

topic(Irc, Channel, Topic) ->
	command(Irc, {topic, Channel, Topic}).

get_channels(Irc) ->
	sync_command(Irc, get_channels).

get_channels_info(Irc) ->
	sync_command(Irc, get_channels_info).

get_channel_info(Irc, Chan) ->
	sync_command(Irc, {get_channel_info, Chan}).	

%% Call Fun for each channel we're online
each_channel(Irc, Fun) ->
	[Fun(Chan) || Chan <- get_channels(Irc)].

%% Call Fun for each channel where specified nick is online
each_channel(Irc, Fun, Nick) ->
	[Fun(Chan) || {Chan, _, Users} <- get_channels_info(Irc), is_user_present(Nick, Users)].

is_user_present(Irc, Nick, Chan) ->
	{_, _, Users} = get_channel_info(Irc, Chan),
	is_user_present(Nick, Users).

is_user_present(Nick, [{_, Nick, _} | _]) ->
	true;
is_user_present(Nick, [_ | Rest]) ->
	is_user_present(Nick, Rest);
is_user_present(_, []) ->
	false.

%% Send big bulk of private/channel messages asynchronously
async_chanmsg(Irc, Channel, Lines) ->
	async_command(Irc, {async_irc_command, chanmsg, Channel, Lines}).

async_action(Irc, Channel, Lines) ->
	async_command(Irc, {async_irc_command, action, Channel, Lines}).

async_privmsg(Irc, To, Lines) ->
	async_command(Irc, {async_irc_command, privmsg, To, Lines}).

%% gen_fsm callbacks

init({Host, Nick, Handler, Options}) ->
	process_flag(trap_exit, true),
	Conf = conf({Nick, Options}),
	conn_throttle(Host, Conf),
	{ok, IrcRef} = irc_proto:start_link(Host, Options),
	{ok, state_connecting, #conn{nick = Nick, login = Conf#conf.login, conf = Conf, irc_ref = IrcRef, handler = Handler}}.

conf({Nick, Options}) ->
	%% login and long_name default to nick
	conf(#conf{nick = Nick, login = Nick, long_name = Nick}, Options).

conf(Conf, [Option | Options]) ->
	NewConf = case Option of
		{login, Login} ->
			Conf#conf{login = Login};
		{long_name, LongName} ->
			Conf#conf{long_name = LongName};
		{oper_pass, OperPass} ->
			Conf#conf{oper_pass = OperPass};
		{autojoin, Autojoin} ->
			Conf#conf{autojoin = Autojoin};
		{umode, Umode} ->
			Conf#conf{umode = Umode};
		{conn_rate, MaxConn, Period} ->
			Conf#conf{conn_rate = {MaxConn, Period}};
		{msg_interval, MsgInterval} ->
			Conf#conf{msg_interval = MsgInterval};
		_ ->
			Conf
	end,
	conf(NewConf, Options);
conf(Conf, []) ->
	Conf.

conn_throttle(Host, #conf{conn_rate = {MaxConn, Period}}) ->
	throttle:wait(Host, MaxConn, Period,
				  fun (Delay) ->
						  io:format("Connecting to often, delaying for ~p msec...~n", [Delay]),
						  Delay
				  end).

handle_info({'DOWN', _, process, Pid, _}, StateName, StateData) ->
	Channels = dict:filter(fun (_, V) when V =:= Pid -> false; (_, _) -> true end, StateData#conn.chan_fsms),
	{next_state, StateName, StateData#conn{chan_fsms = Channels}};
handle_info({'EXIT', _, Reason}, _StateName, StateData) when Reason =/= normal ->
	{stop, Reason, StateData};
handle_info({irc, IrcRef, Event}, StateName, #conn{irc_ref = IrcRef} = StateData) ->
	io:format("event from ~p: ~p~n", [IrcRef, Event]),
	send_event(Event),
	{next_state, StateName, StateData};
handle_info(_Info, StateName, StateData) ->
	io:format("INFO: ~p~n", [_Info]),
	{next_state, StateName, StateData}.

terminate(Reason, _StateName, _StateData) ->
	io:format("OOPS: ~p (~p) terminating with reason ~p~n", [?MODULE, self(), Reason]).

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% events that are handled via send_all_state_event
-define(IS_ALLSTATE_EVENT(Event), 
		element(1, Event) =:= ping).

send_event(Event) when ?IS_ALLSTATE_EVENT(Event) ->
	gen_fsm:send_all_state_event(self(), Event);
send_event(Event) ->
	gen_fsm:send_event(self(), Event).

%% Conns

handle_event({ping, _, Server}, StateName, StateData) ->
	{next_state, StateName, pong(Server, StateData)};
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

state_connecting({notice, _, _}, Conn) ->
	{next_state, state_auth_nick, auth_login(Conn)};
state_connecting(_, Conn) ->
	{next_state, state_connecting, Conn}.

state_auth_nick(Event, Conn) when element(1, Event) =:= erroneusnickname;
								   element(1, Event) =:= nicknameinuse ->
	{next_state, state_auth_nick, auth_login(next_nick(Conn))};
state_auth_nick({myinfo, _, _, _, _, _}, #conn{conf = Conf} = Conn) ->
	case Conf#conf.oper_pass of
		[] ->
			{next_state, state_auth_end, Conn};
		OperPass ->
			{next_state, state_auth_oper, auth_oper(Conn, OperPass)}
	end;
state_auth_nick(_, Conn) ->
	{next_state, state_auth_nick, Conn}.

state_auth_oper(Event, Conn) when element(1, Event) =:= nooperhost;
								   element(1, Event) =:= passwdmismatch -> 
	{next_state, state_connected, autojoin(Conn)};
state_auth_oper({youreoper, _, _}, Conn) ->
	{next_state, state_connected, autojoin(retry_nick(set_umode(Conn#conn{is_oper = true})))};
state_auth_oper(_, Conn) ->
	{next_state, state_auth_oper, Conn}.

state_auth_end({endofmotd, _, _}, Conn) ->
	{next_state, state_connected, autojoin(Conn)};
state_auth_end(_, Conn) ->
	{next_state, state_auth_end, Conn}.

state_connected({async_irc_command, privmsg, To, Lines}, Conn) ->
	spawn_link(fun () ->
					   [begin irc_command({privmsg, To, Line}, Conn), 
							  msg_throttle(Conn)
						end || Line <- Lines],
					   ok
			   end),
	{next_state, state_connected, Conn};	
state_connected({async_irc_command, Cmd, Channel, Lines}, Conn) ->
	case dict:find(Channel, Conn#conn.chan_fsms) of
		{ok, FsmRef} ->
			Fun = fun (Line) -> irc_command({Cmd, Channel, Line}, Conn),
								msg_throttle(Conn)
				  end,
			irc_chan:chan_event(FsmRef, {irc_command, Fun, Lines});
		_ ->	
			false
	end,
	{next_state, state_connected, Conn};
state_connected({irc_command, Cmd}, Conn) ->
	irc_command(Cmd, Conn),
	{next_state, state_connected, notify_selfevent(Cmd, Conn)};
state_connected(Event, Conn) ->
	{next_state, state_connected, notify_raw_event(Event, Conn)}.

state_connected(get_channels, _From, Conn) ->
	{reply, dict:fetch_keys(Conn#conn.chan_fsms), state_connected, Conn};
state_connected(get_channels_info, _From, Conn) -> 
	Channels = dict:to_list(Conn#conn.chan_fsms),
	ChanInfo = [irc_chan:get_chan_info(FsmRef) || {_, FsmRef} <- Channels],
	{reply, ChanInfo, state_connected, Conn};
state_connected({get_channel_info, Chan}, _From, Conn) ->
	case dict:find(Chan, Conn#conn.chan_fsms) of 
		{ok, FsmRef} ->
			{reply, irc_chan:get_chan_info(FsmRef), state_connected, Conn};
		Error ->
			{reply, Error, state_connected, Conn}
	end.

myevent({privmsg, Target, User, Msg}, Nick) when Target /= Nick ->
	{chanmsg, Target, User, Msg};
myevent({privmsg, Nick, User, Msg}, Nick) ->
	{privmsg, User, Msg};
myevent({join, Channel, ?USER(Nick)}, Nick) ->
	{joining, Channel};  % joined will be sent by channel FSM after ENDOFNAMES
myevent({part, Channel, ?USER(Nick), _}, Nick) ->
	{parted, Channel};
myevent({kick, Channel, User, Nick, Reason}, Nick) ->
	{kicked, Channel, User, Reason};
myevent({mode, Channel, User, Mode, Nick}, Nick) ->
	{mymode, Channel, User, Mode, Nick};
%% User mode event can ONLY be myevent anyway
myevent({umode, Nick, Mode}, Nick) ->
	{umode, Nick, Mode};
myevent({nick, NewNick, ?USER(Nick)}, Nick) ->
	{mynick, NewNick};
myevent({topic, Channel, ?USER(Nick), Topic}, Nick) ->
	{mytopic, Channel, Nick, Topic};
myevent(Event, _) ->
	Event.

notify_raw_event(Event, Conn) ->
	notify_event(myevent(Event, Conn#conn.nick), Conn).

%% events that are propagated to channel FSM (channel name must be 2nd element in event tuple)
-define(IS_CHAN_EVENT(Event), 
		element(1, Event) =:= joining; 
		element(1, Event) =:= chantopic; 
		element(1, Event) =:= names; 
		element(1, Event) =:= endofnames; 
		element(1, Event) =:= parted; 
		element(1, Event) =:= kicked; 
		element(1, Event) =:= join; 
		element(1, Event) =:= part; 
		element(1, Event) =:= kick; 
		element(1, Event) =:= mode; 
		element(1, Event) =:= mymode; 
		element(1, Event) =:= topic; 
		element(1, Event) =:= mytopic).

%% events that are propagated to ALL channel FSMs
-define(IS_ALLCHAN_EVENT(Event),
		element(1, Event) =:= quit;
		element(1, Event) =:= nick).

%% TODO: handle mymode here?..
notify_event({mynick, Nick}, Conn) ->
	Conn#conn{nick = Nick};
notify_event(Event, Conn) when ?IS_CHAN_EVENT(Event) ->
	notify_chanevent(Event, Conn);
notify_event(Event, Conn) when ?IS_ALLCHAN_EVENT(Event) ->
	notify_allchanevent(Event, Conn);
notify_event(Event, Conn) ->
	notify_genevent(Event, Conn).

notify_chanevent({joining, Channel}, Conn) ->
	{ok, Pid} = irc_chan:start_link(Channel),
	erlang:monitor(process, Pid),
	Conn#conn{chan_fsms = dict:store(Channel, Pid, Conn#conn.chan_fsms)};
notify_chanevent(Event, Conn) ->
	Channel = element(2, Event),
	Pid = dict:fetch(Channel, Conn#conn.chan_fsms),
	notify(irc_chan:chan_event(Pid, Event), chanevent, Conn).

notify_allchanevent(Event, Conn) ->
	[irc_chan:chan_event(Pid, Event) || {_, Pid} <- dict:to_list(Conn#conn.chan_fsms)],
	notify_genevent(Event, Conn).

notify_genevent(Event, Conn) ->
	notify(Event, genevent, Conn).

notify_selfevent(Event, Conn) ->
	notify(Event, selfevent, Conn).

notify(noevent, _, Conn) ->
	Conn;
notify(Event, Type, #conn{handler = H,     nick = Nick, login = Login, is_oper = IsOper} = Conn) ->
	H(Type, Event, #irc{conn_ref = self(), nick = Nick, login = Login, is_oper = IsOper}),
	Conn.

irc_command({umode, Mode}, #conn{irc_ref = IrcRef, nick = Nick}) ->
	irc_proto:irc_command(IrcRef, {umode, Nick, Mode});
irc_command(Cmd, #conn{irc_ref = IrcRef}) ->
	irc_proto:irc_command(IrcRef, Cmd).

auth_login(#conn{nick = Nick, login = Login, conf = Conf} = Conn) ->
	irc_command({nick, Nick}, Conn),
	irc_command({user, Login, Conf#conf.long_name}, Conn),
	Conn.

retry_nick(#conn{conf = Conf} = Conn) ->
	irc_command({nick, Conf#conf.nick}, Conn),
	Conn.

auth_oper(Conn, OperPass) ->
	irc_command({oper, Conn#conn.login, OperPass}, Conn),
	Conn.

set_umode(#conn{is_oper = false} = Conn) ->
	Conn;  % must be oper
set_umode(#conn{conf = #conf{umode = []}} = Conn) ->
	Conn;
set_umode(#conn{conf = Conf} = Conn) ->
	irc_command({umode, Conf#conf.umode}, Conn),
	Conn.

next_nick(#conn{conf = Conf, nick_suffix = Suffix} = Conn) ->
	Conn#conn{nick = Conf#conf.nick ++ "_" ++ integer_to_list(Suffix), nick_suffix = Suffix + 1}.

autojoin(#conn{conf = Conf} = Conn) ->
	autojoin(Conn, Conf#conf.autojoin).

autojoin(Conn, [Channel | Rest]) ->
	irc_command({join, Channel}, Conn),
	autojoin(Conn, Rest);
autojoin(Conn, []) ->
	Conn.

msg_throttle(#conn{conf = #conf{msg_interval = T}}) ->
	timer:sleep(T).

pong(Server, Conn) ->
	irc_command({pong, Server}, Conn),
	Conn.
