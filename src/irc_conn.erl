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
-export([bulk_chanmsg/3, bulk_action/3, bulk_privmsg/3]).

-record(conf, {nick       = [] :: list(),     %% initial nick requested
			   login      = [] :: list(),     %% login field in USER and OPER commands (defaults to nick)
			   long_name  = [] :: list(),     %% long name in USER command (defaults to nick)
			   oper_pass  = [] :: list(),     %% password in OPER command (won't do OPER if not specified)
			   umode      = [] :: list(),     %% umode spec to request initially (like, "+F")
			   impl_umode = [] :: list(),     %% umode spec which is implied (used as workaround for absent +F)
			   autojoin   = [] :: [list()],   %% channels to join automatically
			   msg_interval = 200,            %% minimal interval between messages when sending long bulks
			   conn_rate    = {2, 16000}}).   %% maximum connection rate

-record(conn, {nick                   :: list(),    %% actual nick (may differ from initially requested one)
			   nick_suffix = 1        :: integer(), %% suffix appended to nick on nick collision
			   login                  :: list(),    %% actual login used
			   is_oper   = false      :: boolean(),
			   umode     = []         :: [atom()],  %% actual umodes set
			   conf                   :: #conf{},
			   irc_ref                :: pid(),
			   handler                :: function(),
			   chan_fsms = dict:new() :: dict()}).  %% channame -> pid()

-include("irc.hrl").

%%% Starting

start(Handler, {Host, Nick, Options}) ->
	gen_fsm:start(?MODULE, {Host, Nick, Handler, Options}, []).
start(FsmName, Handler, {Host, Nick, Options}) ->
	gen_fsm:start(FsmName, ?MODULE, {Host, Nick, Handler, Options}, []).

start_link(Handler, {Host, Nick, Options}) ->
	gen_fsm:start_link(?MODULE, {Host, Nick, Handler, Options}, []).
start_link(FsmName, Handler, {Host, Nick, Options}) ->
	gen_fsm:start_link(FsmName, ?MODULE, {Host, Nick, Handler, Options}, []).

%%% Internal
%%% Irc may be #irc{} or FSM pid/name

sync_send_command(#irc{conn_ref = FsmRef}, Cmd) ->
	gen_fsm:sync_send_event(FsmRef, Cmd, infinity);
sync_send_command(FsmRef, Cmd) when is_pid(FsmRef); is_atom(FsmRef) ->
	gen_fsm:sync_send_event(FsmRef, Cmd, infinity).

async_send_command(#irc{conn_ref = FsmRef}, Cmd) ->
	gen_fsm:send_event(FsmRef, Cmd);
async_send_command(FsmRef, Cmd) when is_pid(FsmRef); is_atom(FsmRef) ->
	gen_fsm:send_event(FsmRef, Cmd).

%%% IRC commands

%% chanmsg and privmsg are actually aliases -- real message type is determined by target (nick/channel)
%% IRC itself doesn't distinguish them so irc_proto collides them back
%% This discrimination is needed for correct selfevent handling

command(Irc, Command)            -> async_send_command(Irc, {irc_command, Command}).
chanmsg(Irc, Channel, Msg)       -> async_send_command(Irc, {irc_command, {msgtype(chanmsg, Channel), Channel, Msg}}).
privmsg(Irc, Nick, Msg)          -> async_send_command(Irc, {irc_command, {msgtype(privmsg, Nick), Nick, Msg}}).
action(Irc, Channel, Action)     -> async_send_command(Irc, {irc_command, {msgtype(action, Channel), Channel, Action}}).
join(Irc, Channel)               -> async_send_command(Irc, {irc_command, {join, Channel}}).
part(Irc, Channel)               -> async_send_command(Irc, {irc_command, {part, Channel}}).
quit(Irc, QuitMsg)               -> async_send_command(Irc, {irc_command, {quit, QuitMsg}}).
mode(Irc, Channel, User, Mode)   -> async_send_command(Irc, {irc_command, {mode, Channel, User, Mode}}).
umode(Irc, Mode)                 -> async_send_command(Irc, {irc_command, {umode, Mode}}).
nick(Irc, Nick)                  -> async_send_command(Irc, {irc_command, {nick, Nick}}).
kick(Irc, Channel, Nick, Reason) -> async_send_command(Irc, {irc_command, {kick, Channel, Nick, Reason}}).
topic(Irc, Channel, Topic)       -> async_send_command(Irc, {irc_command, {topic, Channel, Topic}}).

%% Send big bulk of private/channel messages asynchronously
bulk_chanmsg(Irc, Channel, Lines) -> async_send_command(Irc, {bulk_irc_command, msgtype(chanmsg, Channel), Channel, Lines}).
bulk_action(Irc, Channel, Lines)  -> async_send_command(Irc, {bulk_irc_command, msgtype(action, Channel), Channel, Lines}).
bulk_privmsg(Irc, Nick, Lines)    -> async_send_command(Irc, {bulk_irc_command, msgtype(privmsg, Nick), Nick, Lines}).

%% Determine real message type (action/chan/priv) based on target (nick/channel)
msgtype(action, Chan)  when ?IS_CHAN(Chan) -> action;
msgtype(_, Chan)       when ?IS_CHAN(Chan) -> chanmsg;
msgtype(_, _)                              -> privmsg.

%%% IRC queries

get_channels(Irc)           -> sync_send_command(Irc, get_channels).
get_channels_info(Irc)      -> sync_send_command(Irc, get_channels_info).
get_channel_info(Irc, Chan) -> sync_send_command(Irc, {get_channel_info, Chan}).	

is_user_present(Irc, Nick, Chan) ->
	{_, _, Users} = get_channel_info(Irc, Chan),
	is_user_present(Nick, Users).

is_user_present(Nick, [{_, Nick, _} | _]) -> true;
is_user_present(Nick, [_ | Rest])         -> is_user_present(Nick, Rest);
is_user_present(_, [])                    -> false.

%% Call Fun for each channel we're online
each_channel(Irc, Fun) -> [Fun(Chan) || Chan <- get_channels(Irc)].

%% Call Fun for each channel where specified nick is online
each_channel(Irc, Fun, Nick) ->
	[Fun(Chan) || {Chan, _, Users} <- get_channels_info(Irc), is_user_present(Nick, Users)].

%%% gen_fsm callbacks

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
		{impl_umode, Umode} ->
			Conf#conf{impl_umode = Umode};
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
		element(1, Event) =:= ping;
		element(1, Event) =:= umode).

send_event(Event) when ?IS_ALLSTATE_EVENT(Event) ->
	gen_fsm:send_all_state_event(self(), Event);
send_event(Event) ->
	gen_fsm:send_event(self(), Event).

%%% Generic event handlers

handle_event({ping, _, Server}, StateName, StateData) ->
	{next_state, StateName, pong(Server, StateData)};
handle_event(Event, StateName, StateData) ->
	{next_state, StateName, process_event_raw(Event, StateData)}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

%%% Per-state event handlers

state_connecting({notice, _, _}, #conn{conf = Conf} = Conn) ->
	{next_state, state_auth_nick, auth_login(apply_umode(Conf#conf.impl_umode, Conn))};
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

state_connected({bulk_irc_command, privmsg, Nick, Lines}, Conn) ->
	spawn_link(fun () ->
					   Fun = fun (Line) -> do_irc_command({privmsg, Nick, Line}, Conn),
										   msg_throttle(Conn)
							 end,
					   [Fun(Line) || Line <- Lines]
			   end),
	{next_state, state_connected, Conn};	
state_connected({bulk_irc_command, Cmd, Channel, Lines}, Conn) ->
	case dict:find(Channel, Conn#conn.chan_fsms) of
		{ok, FsmRef} ->
			Fun = fun (Line) -> do_irc_command({Cmd, Channel, Line}, Conn),
								msg_throttle(Conn)
				  end,
			irc_chan:chan_event(FsmRef, {bulk_irc_command, Fun, Lines});
		_ ->	
			false
	end,
	{next_state, state_connected, Conn};
state_connected({irc_command, Cmd}, Conn) ->
	{next_state, state_connected, do_irc_command(Cmd, Conn)};
state_connected(Event, Conn) ->
	{next_state, state_connected, process_event_raw(Event, Conn)}.

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

%% Processes raw event (as it is received from IRC)
process_event_raw(Event, Conn) ->
	process_event(myevent(Event, Conn#conn.nick), Conn).

%% Transforms events targeted to self (own nick change, etc) to "myevents"
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

%% Processes IRC event after "myevent" transform (may change state and/or propagate event)
%% Events are divided into 3 type: 
%% - channel events (sent to channel FSM, reply is propagated as event)
%% - generic events (propagated directly)
%% - all-channel events (sent to all channel FSMs, all replies are propagated)
%% - selfevents (propagated by commands executed)
process_event({mynick, Nick}, Conn) ->
	Conn#conn{nick = Nick};
process_event({umode, _, Mode}, Conn) ->
	apply_umode(Mode, Conn);
process_event(Event, Conn) when ?IS_CHAN_EVENT(Event) ->
	notify_chanevent(Event, Conn);
process_event(Event, Conn) when ?IS_ALLCHAN_EVENT(Event) ->
	notify_allchanevent(Event, Conn);
process_event(Event, Conn) ->
	notify_genevent(Event, Conn).

%% Sends event to all channel FSMs, gathers replies and resends them to handler
notify_allchanevent(Event, Conn) ->
	EvList = [irc_chan:chan_event(Pid, Event) || {_, Pid} <- dict:to_list(Conn#conn.chan_fsms)],
	notify_chanreplies(EvList, Conn).

%% `joining' is a special event that starts new channel FSM
notify_chanevent({joining, Channel}, Conn) ->
	{ok, Pid} = irc_chan:start_link(Channel),
	erlang:monitor(process, Pid),
	Conn#conn{chan_fsms = dict:store(Channel, Pid, Conn#conn.chan_fsms)};
%% Any other `chanevent' is propagated to FSM as is
notify_chanevent(Event, Conn) ->
	Channel = element(2, Event),
	Pid = dict:fetch(Channel, Conn#conn.chan_fsms),
	notify_chanreplies([irc_chan:chan_event(Pid, Event)], Conn).
	%notify(irc_chan:chan_event(Pid, Event), chanevent, Conn).

%% Events received from channel FSMs may be either `chanevent' or `genevent'
%% `chanevent' must have channel name as it's 2nd element.
%% It will NOT be propagated to channel FSM again (as it was received directly from it)
notify_chanreplies([Event | Rest], Conn) when ?IS_CHAN(element(2, Event)) ->
	notify_chanreplies(Rest, notify(Event, chanevent, Conn));
notify_chanreplies([Event | Rest], Conn) ->
	notify_chanreplies(Rest, notify_genevent(Event, Conn));
notify_chanreplies([], Conn) ->
	Conn.

%% Generic events are sent directly to notifier function
notify_genevent(Event, Conn) ->
	notify(Event, genevent, Conn).

%% Each IRC command performed is sent back to handler as `selfevent'
notify_selfevent(Event, Conn) ->
	notify(Event, selfevent, Conn).

%% Propagate event to notifier
notify(noevent, _, Conn) ->
	Conn;
notify(Event, Type, #conn{handler = H,     nick = Nick, login = Login, is_oper = IsOper} = Conn) ->
	H(Type, Event, #irc{conn_ref = self(), nick = Nick, login = Login, is_oper = IsOper}),
	Conn.

do_irc_command({umode, Mode}, #conn{nick = Nick} = Conn) ->
	do_irc_command({umode, Nick, Mode}, Conn);
do_irc_command(Cmd, #conn{irc_ref = IrcRef} = Conn) ->
	irc_proto:send_irc_command(IrcRef, Cmd),
	notify_selfevent(Cmd, Conn).

auth_login(#conn{nick = Nick, login = Login, conf = Conf} = Conn) ->
	do_irc_command({nick, Nick}, Conn),
	do_irc_command({user, Login, Conf#conf.long_name}, Conn).

retry_nick(#conn{conf = Conf} = Conn) ->
	do_irc_command({nick, Conf#conf.nick}, Conn).

auth_oper(Conn, OperPass) ->
	do_irc_command({oper, Conn#conn.login, OperPass}, Conn).

set_umode(#conn{is_oper = false} = Conn) ->
	Conn;  % must be oper
set_umode(#conn{conf = #conf{umode = []}} = Conn) ->
	Conn;
set_umode(#conn{conf = Conf} = Conn) ->
	do_irc_command({umode, Conf#conf.umode}, Conn).

next_nick(#conn{conf = Conf, nick_suffix = Suffix} = Conn) ->
	Conn#conn{nick = Conf#conf.nick ++ "_" ++ integer_to_list(Suffix), nick_suffix = Suffix + 1}.

autojoin(#conn{conf = Conf} = Conn) ->
	autojoin(Conn, Conf#conf.autojoin).

autojoin(Conn, [Channel | Rest]) ->
	do_irc_command({join, Channel}, Conn),
	autojoin(Conn, Rest);
autojoin(Conn, []) ->
	Conn.

-define(MIN_MSG_THROTTLE, 1).  %% Throttle delay (in msec) used when no flood restrictions are appplied.

msg_throttle(#conn{conf = #conf{msg_interval = T}, umode = M}) ->
	case lists:member(nofloodlimits, M) of
		true  -> timer:sleep(?MIN_MSG_THROTTLE);
		false -> timer:sleep(T)
	end.

pong(Server, Conn) ->
	do_irc_command({pong, Server}, Conn).

apply_umode([$- | Modes], Conn) ->
	remove_umodes(Modes, Conn#conn.umode, Conn);
apply_umode([$+ | Modes], Conn) ->
	add_umodes(Modes, Conn#conn.umode, Conn);
apply_umode(Modes, Conn) ->
	add_umodes(Modes, Conn#conn.umode, Conn).

add_umodes([], Current, Conn) ->
	Conn#conn{umode = Current};
add_umodes([Mode | Rest], Current, Conn) ->
	case irc_modes:umode_to_atom(Mode) of 
		undefined ->
			add_umodes(Rest, Current, Conn);
		M ->
			add_umodes(Rest, util:set_flag(M, Current), Conn)
	end.

remove_umodes([], Current, Conn) ->
	Conn#conn{umode = Current};
remove_umodes([Mode | Rest], Current, Conn) ->
	case irc_modes:umode_to_atom(Mode) of 
		undefined ->
			remove_umodes(Rest, Current, Conn);
		M ->
			remove_umodes(Rest, util:unset_flag(M, Current), Conn)
	end.

