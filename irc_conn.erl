-module(irc_conn).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {eventmgr :: pid() | atom() | {atom(),atom()},
				nick     :: list(),
				channels :: [list()],
				sock     :: port(),
				ping     :: integer()}).

%% public interface
-export([start_link/5]).
-export([chanmsg/3, privmsg/3, join/2, pong/2, part/2, quit/2, action/2, 
		 mode/4, mode/3, user/3, nick/2, oper/3, kick/4, topic/3]).

-include("irc_conn.hrl").

%% public interface

start_link(ServerName, Host, Port, EventMgr, Options) ->
	case ServerName of
		undef ->
			gen_server:start_link(?MODULE, {Host, Port, Options, EventMgr}, [{timeout, infinite}]);
		_ ->
			gen_server:start_link(ServerName, ?MODULE, {Host, Port, Options, EventMgr}, [{timeout, infinite}])
	end.

chanmsg(Conn, Channel, Msg) ->
	call(Conn, {chanmsg, Channel, Msg}).

privmsg(Conn, To, Msg) ->
	call(Conn, {privmsg, To, Msg}).

join(Conn, Channel) ->
	call(Conn, {join, Channel}).

pong(Conn, Server) ->
	call(Conn, {pong, Server}).

part(Conn, Channel) ->
	call(Conn, {part, Channel}).

quit(Conn, QuitMsg) ->
	call(Conn, {quit, QuitMsg}).

action(Conn, Action) ->
	call(Conn, {action, Action}).

mode(Conn, Channel, User, Mode) ->
	call(Conn, {mode, Channel, User, Mode}).

mode(Conn, User, Mode) ->
	call(Conn, {mode, User, Mode}).

user(Conn, Login, LongName) ->
	call(Conn, {user, Login, LongName}).

nick(Conn, Nick) ->
	call(Conn, {nick, Nick}).

oper(Conn, Login, Passwd) ->
	call(Conn, {oper, Login, Passwd}).

kick(Conn, Channel, Nick, Reason) ->
	call(Conn, {kick, Channel, Nick, Reason}).

topic(Conn, Channel, Topic) ->
	call(Conn, {topic, Channel, Topic}).

call(Conn, Req) ->
	gen_server:call(Conn, Req, infitine).

%% gen_server callbacks

init({Host, Port, Options, EventMgr}) ->
	State = connect(Host, Port, Options),
	{ok, State#state{eventmgr = EventMgr}, State#state.ping}.

keyfind(Key, List, Default) ->
	case lists:keyfind(Key, List) of
		false ->
			Default;
		Any ->
			Any
	end.

connect(Host, Port, Options) ->
	SockTimeout = keyfind(sock_timeout, Options, ?SOCK_TIMEOUT),
	RetryTimeout = keyfind(retry_timeout, Options, ?RETRY_TIMEOUT),
	PingTimeout = keyfind(ping_timeout, Options, ?PING_TIMEOUT),
	case gen_tcp:connect(Host, Port, [{packet, line},
									  {send_timeout, SockTimeout},
									  {send_timeout_close, true}], 
						 SockTimeout) of
		{ok, Sock} ->
			#state{sock = Sock, ping = PingTimeout};
		{error, timeout} ->
			timer:sleep(RetryTimeout),
			connect(Host, Port, Options);
		{error, _} ->
			connect(Host, Port, Options)
	end.

handle_call(Req, _From, State) ->
	{reply, ok, do_command(Req), State#state.ping}.

handle_cast(_Req, State) ->
	{noreply, State, State#state.ping}.

handle_info(timeout, State) ->
	{stop, timeout, State};
handle_info({tcp_closed, Sock}, State) when Sock == State#state.sock ->
	{stop, tcp_closed, State};
handle_info({tcp_error, Sock, Reason}, State) when Sock == State#state.sock ->
	{stop, {tcp_error, Reason}, State};
handle_info({tcp, Sock, Data}, State) when Sock == State#state.sock ->
	{Event, NewState} = parse_data(Data, State),
	gen_event:notify(NewState#state.eventmgr, Event),
	{noreply, NewState, NewState#state.ping}.

terminate(_Reason, State) ->
	gen_tcp:close(State#state.sock).

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

%% IRC protocol commands

do_command({chanmsg, State, Channel, Msg}) ->
	do_command({privmsg, State, Channel, Msg});
do_command({privmsg, State, To, Msg}) ->
	send(State, "PRIVMSG " ++ To ++ " :" ++ Msg);
do_command({join, State, Channel}) ->
	send(State, "JOIN :" ++ Channel);
do_command({pong, State, Server}) ->
	send(State, "PONG " ++ Server);
do_command({part, State, Channel}) ->
	send(State, "PART " ++ Channel);
do_command({quit, State, QuitMsg}) ->
	send(State, "QUIT " ++ QuitMsg);
do_command({action, State, Action}) ->
	do_command({chanmsg, State, [1] ++ "ACTION " ++ Action ++ [1]});
do_command({mode, State, Channel, User, Mode}) ->
	send(State, "MODE " ++ Channel ++ " " ++ Mode ++ " " ++ User);
do_command({mode, State, User, Mode}) ->
	send(State, "MODE " ++ User ++ " " ++ Mode);
do_command({user, State, Login, LongName}) ->
	send(State, "USER " ++ Login ++ " 8 * :" ++ LongName);
do_command({nick, State, Nick}) ->
	send(State, "NICK " ++ Nick);
do_command({oper, State, Login, Passwd}) ->
	send(State, "OPER " ++ Login ++ " " ++ Passwd);
%% do_command({login, State, Nick, Login, LongName}) ->
%% 	try_nick(State, Nick),
%% 	user(State, Login, LongName);
%% do_command({login, State, Nick, Login, Passwd, LongName}) ->
%% 	try_nicn(State, Nick),
%% 	user(State, Login, LongName),
%% 	oper(State, Login, Passwd);
%% do_command({login, State, Nick, Login, Passwd, LongName}) ->
%% 	try_nick, State, Nick),
%% 	user(State, Login, LongName),
%% 	oper(State, Login, Passwd),
%% 	nick(State, Nick);
do_command({kick, State, Channel, Nick, Reason}) ->
	send(State, "KICK " ++ Channel ++ " " ++ Nick ++ " :" ++ Reason);
do_command({topic, State, Channel, Topic}) ->
	send(State, "TOPIC " ++ Channel ++ " :" ++ Topic).

send(State, Data) ->
	ok = gen_tcp:send(State#state.sock, Data ++ ?CRLF),
	State.

%% IRC protocol parser

parse_data(_Data, State) ->
	State.
