-module(irc_conn).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_fsm
-behaviour(gen_fsm).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).
-export([state_not_connected/2, state_connecting/2, state_auth_nick/2, state_auth_oper/2, 
		 state_auth_end/2, state_connected/2, state_connected/3]).

%% public interface
-export([start_link/0, connect/0]).
-export([chanmsg/3, privmsg/3, action/3, join/1, part/1, quit/1, mode/3, umode/1, kick/3, topic/2, nick/1]).
-export([get_channels_info/0, get_channels/0, get_channel_info/1]).
-export([for_each_channel/1, for_each_channel/2, is_user_present/2]).
-export([bulk_chanmsg/3, bulk_action/3, bulk_privmsg/3]).

%% -record(conf, {nick       = [] :: list(),     %% initial nick requested
%% 			   login      = [] :: list(),     %% login field in USER and OPER commands (defaults to nick)
%% 			   long_name  = [] :: list(),     %% long name in USER command (defaults to nick)
%% 			   oper_pass  = [] :: list(),     %% password in OPER command (won't do OPER if not specified)
%% 			   umode      = [] :: list(),     %% umode spec to request initially (like, "+F")
%% 			   pretend_umode = [] :: list(),     %% umode spec which is implied (used as workaround for absent +F)
%% 			   autojoin   = [] :: [list()],   %% channels to join automatically
%% 			   msg_interval = 200,            %% minimal interval between messages when sending long bulks
%% 			   conn_rate    = {2, 16000}}).   %% maximum connection rate

-record(conn, {nick                      :: list(),    %% actual nick (may differ from initially requested one)
			   nick_suffix = 1           :: integer(), %% suffix appended to nick on nick collision
			   login                     :: list(),    %% actual login used
			   is_oper   = false         :: boolean(),
			   umode     = []            :: [atom()],  %% actual umodes set
			   irc_proto_ref             :: pid(),
			   priv_senders = dict:new() :: dict(),    %% bulk privmsg sender processes, nick -> pid()
			   chan_fsms = dict:new()    :: dict()}).  %% channame -> pid()

-include("irc.hrl").

%%% API

start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Initiate connection
connect() ->
	gen_fsm:send_event(?MODULE, connect).

%%% IRC commands

%% chanmsg and privmsg are actually aliases -- real message type is determined by target (nick/channel)
%% IRC itself doesn't distinguish them so irc_proto_ref collides them back
%% This discrimination is needed for correct selfevent handling

%% All message commands (privmsg/chanmsg/action) have Save modifier:
%% - `hist' -- save to history
%% - `nohist' -- do not save
%% This modifier is stripped before passing to `irc_proto_ref' but present in selfevents

privmsg(Nick, Save, Msg)      -> gen_fsm:send_event(?MODULE, {irc_command, {msgtype(privmsg, Nick), Nick, Save, Msg}}).
chanmsg(Channel, Save, Msg)   -> gen_fsm:send_event(?MODULE, {irc_command, {msgtype(chanmsg, Channel), Channel, Save, Msg}}).
action(Channel, Save, Action) -> gen_fsm:send_event(?MODULE, {irc_command, {msgtype(action, Channel), Channel, Save, Action}}).
join(Channel)                 -> gen_fsm:send_event(?MODULE, {irc_command, {join, Channel}}).
part(Channel)                 -> gen_fsm:send_event(?MODULE, {irc_command, {part, Channel}}).
quit(QuitMsg)                 -> gen_fsm:send_event(?MODULE, {irc_command, {quit, QuitMsg}}).
mode(Channel, User, Mode)     -> gen_fsm:send_event(?MODULE, {irc_command, {mode, Channel, User, Mode}}).
umode(Mode)                   -> gen_fsm:send_event(?MODULE, {irc_command, {umode, Mode}}).
nick(Nick)                    -> gen_fsm:send_event(?MODULE, {irc_command, {nick, Nick}}).
kick(Channel, Nick, Reason)   -> gen_fsm:send_event(?MODULE, {irc_command, {kick, Channel, Nick, Reason}}).
topic(Channel, Topic)         -> gen_fsm:send_event(?MODULE, {irc_command, {topic, Channel, Topic}}).

%% Send big bulk of private/channel messages asynchronously
bulk_chanmsg(Channel, Save, Lines) -> 
	gen_fsm:send_event(?MODULE, {bulk_irc_command, msgtype(chanmsg, Channel), Channel, Save, Lines}).
bulk_action(Channel, Save, Lines)  -> 
	gen_fsm:send_event(?MODULE, {bulk_irc_command, msgtype(action, Channel), Channel, Save, Lines}).
bulk_privmsg(Nick, Save, Lines)    -> 
	gen_fsm:send_event(?MODULE, {bulk_irc_command, msgtype(privmsg, Nick), Nick, Save, Lines}).

%%% IRC queries

get_channels()         -> gen_fsm:sync_send_event(?MODULE, get_channels, infinity).
get_channels_info()	   -> gen_fsm:sync_send_event(?MODULE, get_channels_info, infinity).
get_channel_info(Chan) -> gen_fsm:sync_send_event(?MODULE, {get_channel_info, Chan}, infinity).	

is_user_present(Nick, Chan) ->
	{_, _, Users} = get_channel_info(Chan),
	find_user(Nick, Users).

%% Call Fun for each channel we're online
for_each_channel(Fun) -> [Fun(Chan) || Chan <- get_channels()].

%% Call Fun for each channel where specified nick is online
for_each_channel(Fun, Nick) ->
	[Fun(Chan) || {Chan, _, Users} <- get_channels_info(), find_user(Nick, Users)].

%%% gen_fsm callbacks

init(_) ->
	{ok, IrcPid} = irc_proto:start_link(),
	{ok, state_not_connected, #conn{nick  = erlbot_config:get_value(nick),
									login = erlbot_config:get_value(login), 
									irc_proto_ref = IrcPid}}.

handle_info({'DOWN', _, process, Pid, _}, StateName, StateData) ->
	Channels = dict:filter(fun (_, V) when V =:= Pid -> false; (_, _) -> true end, StateData#conn.chan_fsms),
	Senders  = dict:filter(fun (_, V) when V =:= Pid -> false; (_, _) -> true end, StateData#conn.priv_senders),
	{next_state, StateName, StateData#conn{chan_fsms = Channels, priv_senders = Senders}};
handle_info({'EXIT', _, Reason}, _StateName, StateData) when Reason =/= normal ->
	{stop, Reason, StateData};
handle_info({irc, IrcProto, Event}, StateName, #conn{irc_proto_ref = IrcProto} = StateData) ->
	error_logger:info_report([{irc_event, Event}]),
	send_event(Event),
	{next_state, StateName, StateData};
handle_info(_Info, StateName, StateData) ->
	error_logger:warning_report([{'INFO', _Info}]),
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% events that are handled via send_all_state_event
-define(IS_ALLSTATE_EVENT(Event), 
		element(1, Event) =:= ping;
		element(1, Event) =:= umode).

%% Propagate event received by `handle_info' to self
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

state_not_connected(connect, Conn) ->
	irc_proto:connect(),
	{next_state, state_connecting, Conn}.

state_connecting({notice, _, _}, Conn) ->
	Umode = erlbot_config:get_value(pretend_umode),
	{next_state, state_auth_nick, auth_login(apply_umode(Umode, Conn))};
state_connecting(_, Conn) ->
	{next_state, state_connecting, Conn}.

state_auth_nick(Event, Conn) when element(1, Event) =:= erroneusnickname;
								  element(1, Event) =:= nicknameinuse ->
	{next_state, state_auth_nick, auth_login(next_nick(Conn))};
state_auth_nick({myinfo, _, _, _, _, _}, Conn) ->
	case erlbot_config:get_value(oper_pass) of
		undefined -> {next_state, state_auth_end, Conn};
		OperPass  -> {next_state, state_auth_oper, auth_oper(Conn, OperPass)}
	end;
state_auth_nick(_, Conn) ->
	{next_state, state_auth_nick, Conn}.

state_auth_oper(Event, Conn) when element(1, Event) =:= nooperhost;
								  element(1, Event) =:= passwdmismatch -> 
	{next_state, state_connected, autojoin(Conn)};
state_auth_oper({youreoper, _, _}, Conn) ->
	{next_state, state_connected, autojoin(retry_nick(set_umode(Conn#conn{is_oper = true},
																erlbot_config:get_value(umode))))};
state_auth_oper(_, Conn) ->
	{next_state, state_auth_oper, Conn}.

state_auth_end({endofmotd, _, _}, Conn) ->
	{next_state, state_connected, autojoin(Conn)};
state_auth_end(_, Conn) ->
	{next_state, state_auth_end, Conn}.

state_connected({bulk_irc_command, privmsg, Nick, Save, Lines}, Conn) ->
	{SenderPid, NewConn} = bulk_priv_sender(Nick, Conn),
	Fun = fun (Line) -> do_irc_command({privmsg, Nick, Save, Line}, NewConn),
						msg_throttle(NewConn)
		  end,
	SenderPid ! {bulk_irc_command, Fun, Lines},
	{next_state, state_connected, NewConn};	
state_connected({bulk_irc_command, Cmd, Channel, Save, Lines}, Conn) ->
	case dict:find(Channel, Conn#conn.chan_fsms) of
		{ok, FsmRef} ->
			Fun = fun (Line) -> do_irc_command({Cmd, Channel, Save, Line}, Conn),
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

%%% Internal functions

%% Determine real message type (action/chan/priv) based on target (nick/channel)
msgtype(action, Chan)  when ?IS_CHAN(Chan) -> action;
msgtype(_, Chan)       when ?IS_CHAN(Chan) -> chanmsg;
msgtype(_, _)                              -> privmsg.

find_user(Nick, [{_, Nick, _} | _]) -> true;
find_user(Nick, [_ | Rest])         -> find_user(Nick, Rest);
find_user(_, [])                    -> false.

%% Returns (spawning if needed) PID of bulk privmsg sender for given Nick
%% Sender ceases after some idle timeout
bulk_priv_sender(Nick, Conn) ->
	case dict:find(Nick, Conn#conn.priv_senders) of
		{ok, Pid} -> {Pid, Conn};
		_ ->
			Pid = spawn_link(fun bulk_sender_loop/0),
			erlang:monitor(process, Pid),
			{Pid, Conn#conn{priv_senders = dict:store(Nick, Pid, Conn#conn.priv_senders)}}
	end.

%% Bulk sender loop
bulk_sender_loop() ->
	receive
		{bulk_irc_command, Fun, Lines} -> [Fun(Line) || Line <- Lines],
										  bulk_sender_loop()
	after 1000 -> stop
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
notify(noevent, _, Conn)  -> Conn;
notify(Event, Type, Conn) -> 
	erlbot_ev:notify({Type, Event, #irc{nick = Conn#conn.nick, 
										login = Conn#conn.login, 
										is_oper = Conn#conn.is_oper}}),
	Conn.

%% Perform IRC command (low-level)
%% `umode' needs additional nick (own)
do_irc_command({umode, Mode}, #conn{nick = Nick} = Conn) ->
	do_irc_command({umode, Nick, Mode}, Conn);
%% Strip `hist', `nohist' modifier from message commands
do_irc_command({MsgType, Target, _, Msg} =  Cmd, #conn{irc_proto_ref = IrcRef} = Conn) 
  when MsgType =:= chanmsg; MsgType =:= privmsg; MsgType =:= action ->
	irc_proto:send_irc_command(IrcRef, {MsgType, Target, Msg}),
	notify_selfevent(Cmd, Conn);
do_irc_command(Cmd, #conn{irc_proto_ref = IrcRef} = Conn) ->
	irc_proto:send_irc_command(IrcRef, Cmd),
	notify_selfevent(Cmd, Conn).

auth_login(#conn{nick = Nick, login = Login} = Conn) ->
	do_irc_command({user, Login, erlbot_config:get_value(long_name)}, 
				   do_irc_command({nick, Nick}, Conn)).

retry_nick(Conn) ->
	do_irc_command({nick, erlbot_config:get_value(nick)}, Conn).

auth_oper(Conn, OperPass) ->
	do_irc_command({oper, Conn#conn.login, OperPass}, Conn).

set_umode(#conn{is_oper = false} = Conn, _) -> 
	Conn;  % must be oper
set_umode(Conn, [])    -> Conn;
set_umode(Conn, Umode) -> do_irc_command({umode, Umode}, Conn).

next_nick(#conn{nick_suffix = Suffix} = Conn) ->
	OrigNick = erlbot_config:get_value(nick),
	Conn#conn{nick = OrigNick ++ "_" ++ integer_to_list(Suffix), nick_suffix = Suffix + 1}.

autojoin(Conn) ->
	Channels = erlbot_config:get_value(autojoin),
	lists:foldl(fun (Ch, C) -> do_irc_command({join, Ch}, C) end, Conn, Channels).

-define(MIN_MSG_THROTTLE, 1).  %% Throttle delay (in msec) used when no flood restrictions are appplied.

msg_throttle(#conn{umode = M}) ->
	case lists:member(nofloodlimits, M) of
		true  -> timer:sleep(?MIN_MSG_THROTTLE);
		false -> timer:sleep(erlbot_config:get_value(msg_interval))
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
		undefined -> add_umodes(Rest, Current, Conn);
		M         -> add_umodes(Rest, erlbot_util:set_flag(M, Current), Conn)
	end.

remove_umodes([], Current, Conn) ->
	Conn#conn{umode = Current};
remove_umodes([Mode | Rest], Current, Conn) ->
	case irc_modes:umode_to_atom(Mode) of 
		undefined -> remove_umodes(Rest, Current, Conn);
		M         -> remove_umodes(Rest, erlbot_util:unset_flag(M, Current), Conn)
	end.

