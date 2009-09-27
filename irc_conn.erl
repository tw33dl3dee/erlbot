-module(irc_conn).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_fsm
-behaviour(gen_fsm).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).
-export([state_connecting/2, state_auth_nick/2, state_auth_oper/2, state_auth_end/2, state_connected/2]).

%% gen_irc
-export([handle_irc_event/2]).

%% public interface
-export([start/5, start_link/5]).
-export([test/0, test_op/1]).
-export([chanmsg/3, privmsg/3, join/2, part/2, quit/2, action/3, mode/4, mode/3, kick/4, topic/3, nick/2]).
%-export([user/3, oper/3]).

-record(conf, {nick      = [] :: list(),      %% initial nick requested
			   login     = [] :: list(),      %% login field in USER and OPER commands (defaults to nick)
			   long_name = [] :: list(),      %% long name in USER command (defaults to nick)
			   oper_pass = [] :: list(),      %% password in OPER command (won't do OPER if not specified)
			   autojoin  = [] :: [list()]}).  %% channels to join automatically

-record(state, {nick                   :: list(),    %% actual nick (may differ from initially requested one)
				nick_suffix = 1        :: integer(), %% suffix appended to nick on nick collision
				login                  :: list(),    %% actual login used
				is_oper   = false      :: boolean(),
				conf                   :: #conf{},
				irc_ref                :: pid(),
				handler                :: function(),
				chan_fsms = dict:new() :: dict()}).  %% channame -> pid()

%% public interface

test() ->
	{ok, Irc} = start({local, irc}, "192.168.1.1", "test", fun test_notify/2, [{autojoin, ["#t", "#test"]}]).

test_op(Pass) ->
	{ok, Irc} = start({local, irc}, "192.168.1.1", "dumbot", fun test_notify/2, [{login, "nya"}, {oper_pass, Pass}, {autojoin, ["#t", "#test"]}]).

test_notify(Type, Event) ->
	io:format("*** ~p: ~p~n", [Type, Event]).

start(undef, Host, Nick, Handler, Options) ->
	gen_fsm:start(?MODULE, {Host, Nick, Handler, Options}, []);
start(FsmName, Host, Nick, Handler, Options) ->
	gen_fsm:start(FsmName, ?MODULE, {Host, Nick, Handler, Options}, []).

start_link(undef, Host, Nick, Handler, Options) ->
	gen_fsm:start_link(?MODULE, {Host, Nick, Handler, Options}, []);
start_link(FsmName, Host, Nick, Handler, Options) ->
	gen_fsm:start_link(FsmName, ?MODULE, {Host, Nick, Handler, Options}, []).

%% IRC commands

chanmsg(FsmRef, Channel, Msg) ->
	send_irc_command(FsmRef, {chanmsg, Channel, Msg}).

privmsg(FsmRef, To, Msg) ->
	send_irc_command(FsmRef, {privmsg, To, Msg}).

action(FsmRef, Channel, Action) ->
	send_irc_command(FsmRef, {action, Channel, Action}).

join(FsmRef, Channel) ->
	send_irc_command(FsmRef, {join, Channel}).

part(FsmRef, Channel) ->
	send_irc_command(FsmRef, {part, Channel}).

quit(FsmRef, QuitMsg) ->
	send_irc_command(FsmRef, {quit, QuitMsg}).

mode(FsmRef, Channel, User, Mode) ->
	send_irc_command(FsmRef, {mode, Channel, User, Mode}).

mode(FsmRef, User, Mode) ->
	send_irc_command(FsmRef, {mode, User, Mode}).

nick(FsmRef, Nick) ->
	send_irc_command(FsmRef, {nick, Nick}).

%% user(FsmRef, Login, LongName) ->
%% 	send_irc_command(FsmRef, {user, Login, LongName}).

%% oper(FsmRef, Login, Passwd) ->
%% 	send_irc_command(FsmRef, {oper, Login, Passwd}).

kick(FsmRef, Channel, Nick, Reason) ->
	send_irc_command(FsmRef, {kick, Channel, Nick, Reason}).

topic(FsmRef, Channel, Topic) ->
	send_irc_command(FsmRef, {topic, Channel, Topic}).

send_irc_command(FsmRef, Cmd) ->
	gen_fsm:send_event(FsmRef, {irc_command, Cmd}).

%% gen_fsm callbacks

init({Host, Nick, Handler, Options}) ->
	FsmPid = self(),
	Conf = conf({Nick, Options}),
	{ok, IrcRef} = gen_irc:start_link(undef, fun (Event) -> ?MODULE:handle_irc_event(Event, FsmPid) end, Host, Options),
	{ok, state_connecting, #state{nick = Nick, login = Conf#conf.login, conf = Conf, irc_ref = IrcRef, handler = Handler}}.

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
		_ ->
			Conf
	end,
	conf(NewConf, Options);
conf(Conf, []) ->
	Conf.

handle_info({'DOWN', _, process, Pid, _}, StateName, StateData) ->
	{next_state, StateName, StateData#state{chan_fsms = dict:erase(Pid, StateData#state.chan_fsms)}};
handle_info(_Info, StateName, StateData) ->
	io:format("INFO: ~p~n", [_Info]),
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%% gen_irc callbacks

-define(ALL_STATE_EVENTS, [ping]).

handle_irc_event(Event, FsmPid) when is_tuple(Event) ->
	io:format("event from ~p: ~p~n", [FsmPid, Event]),
	handle_irc_event(Event, FsmPid, lists:member(element(1, Event), ?ALL_STATE_EVENTS)).

handle_irc_event(Event, FsmPid, true) ->
	gen_fsm:send_all_state_event(FsmPid, Event);
handle_irc_event(Event, FsmPid, false) ->
	gen_fsm:send_event(FsmPid, Event).

%% States

handle_event({ping, Server}, StateName, StateData) ->
	{next_state, StateName, pong(Server, StateData)};
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

state_connecting({notice, _}, State) ->
	{next_state, state_auth_nick, auth_login(State)};
state_connecting(_, State) ->
	{next_state, state_connecting, State}.

state_auth_nick(Event, State) when element(1, Event) =:= erroneusnickname;
								   element(1, Event) =:= nicknameinuse ->
	{next_state, state_auth_nick, auth_login(next_nick(State))};
state_auth_nick({myinfo, _, _, _, _}, #state{conf = Conf} = State) ->
	case Conf#conf.oper_pass of
		[] ->
			{next_state, state_auth_end, State};
		OperPass ->
			{next_state, state_auth_oper, auth_oper(State, OperPass)}
	end;
state_auth_nick(_, State) ->
	{next_state, state_auth_nick, State}.

state_auth_oper(Event, State) when element(1, Event) =:= nooperhost;
								   element(1, Event) =:= passwdmismatch -> 
	{next_state, state_connected, autojoin(State)};
state_auth_oper({youreoper, _}, State) ->
	{next_state, state_connected, autojoin(retry_nick(State#state{is_oper = true}))};
state_auth_oper(_, State) ->
	{next_state, state_auth_oper, State}.

state_auth_end({endofmotd, _}, State) ->
	{next_state, state_connected, autojoin(State)};
state_auth_end(_, State) ->
	{next_state, state_auth_end, State}.

state_connected({irc_command, Cmd}, State) ->
	irc_command(Cmd, State),
	{next_state, state_connected, State};
state_connected(Event, State) ->
	{next_state, state_connected, notify_raw_event(Event, State)}.

-define(CHAN_EVENTS, [joining, chantopic, names, endofnames, parted, kicked]).
-define(ME(Nick), {Nick, _, _}).

myevent({privmsg, Target, User, Msg}, Nick) when Target /= Nick->
	{chanmsg, Target, User, Msg};
myevent({join, ?ME(Nick), Channel}, Nick) ->
	{joining, Channel};  % joined will be sent by channel FSM after ENDOFNAMES
myevent({part, ?ME(Nick), Channel, _}, Nick) ->
	{parted, Channel};
myevent({kick, User, Channel, Nick, Reason}, Nick) ->
	{kicked, Channel, User, Reason};
myevent({nick, ?ME(Nick), NewNick}, Nick) ->
	{mynick, NewNick};
myevent(Event, _) ->
	Event.

notify_raw_event(Event, State) ->
	notify_event(myevent(Event, State#state.nick), State).

notify_event(Event, State) ->
	notify_event(Event, lists:member(element(1, Event), ?CHAN_EVENTS), State).

notify_event({mynick, Nick}, _, State) ->
	State#state{nick = Nick};
notify_event(Event, true, State) ->
	notify_chanevent(Event, State);
notify_event(Event, false, State) ->
	notify_genevent(Event, State).

notify_chanevent({joining, Channel}, State) ->
	{ok, Pid} = irc_chan:start_link(Channel),
	erlang:monitor(process, Pid),
	State#state{chan_fsms = dict:store(Channel, Pid, State#state.chan_fsms)};
notify_chanevent(Event, State) ->
	Channel = element(2, Event),
	Pid = dict:fetch(Channel, State#state.chan_fsms),
	notify(irc_chan:chan_event(Pid, Event), chanevent, State).

notify_genevent(Event, State) ->
	notify(Event, genevent, State).

notify(noevent, _, State) ->
	State;
notify(Event, Type, #state{handler = H} = State) ->
	H(Type, Event),
	State.

irc_command(Cmd, #state{irc_ref = IrcRef}) ->
	gen_irc:irc_command(IrcRef, Cmd).

auth_login(#state{nick = Nick, login = Login, conf = Conf} = State) ->
	irc_command({nick, Nick}, State),
	irc_command({user, Login, Conf#conf.long_name}, State),
	State.

retry_nick(#state{conf = Conf} = State) ->
	irc_command({nick, Conf#conf.nick}, State),
	State.

auth_oper(State, OperPass) ->
	irc_command({oper, State#state.login, OperPass}, State),
	State.

next_nick(#state{conf = Conf, nick_suffix = Suffix} = State) ->
	State#state{nick = Conf#conf.nick ++ "_" ++ integer_to_list(Suffix), nick_suffix = Suffix + 1}.

autojoin(#state{conf = Conf} = State) ->
	autojoin(State, Conf#conf.autojoin).

autojoin(State, [Channel | Rest]) ->
	irc_command({join, Channel}, State),
	autojoin(State, Rest);
autojoin(State, []) ->
	State.

pong(Server, State) ->
	irc_command({pong, Server}, State),
	State.

%%% Local Variables:
%%% compile-command: "erlc irc_conn.erl"
%%% End:
