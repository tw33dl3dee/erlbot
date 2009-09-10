-module(irc_conn).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {eventmgr  :: pid() | atom() | {atom(),atom()},
				nick      :: list(),
				channels  :: [list()],
				sock      :: port(),
				ping      :: integer(),
			   	data = [] :: [tuple()]}).

%% public interface
-export([start_link/5]).
-export([chanmsg/3, privmsg/3, join/2, part/2, quit/2, me/2, 
		 mode/4, mode/3, user/3, nick/2, oper/3, kick/4, topic/3]).

-include("irc_conn.hrl").

%% public interface

start_link(undef, Host, Port, EventMgr, Options) ->
	gen_server:start_link(?MODULE, {Host, Port, Options, EventMgr}, [{timeout, infinite}]);
start_link(ServerName, Host, Port, EventMgr, Options) ->
	gen_server:start_link(ServerName, ?MODULE, {Host, Port, Options, EventMgr}, [{timeout, infinite}]).

chanmsg(Conn, Channel, Msg) ->
	call(Conn, {chanmsg, Channel, Msg}).

privmsg(Conn, To, Msg) ->
	call(Conn, {privmsg, To, Msg}).

join(Conn, Channel) ->
	call(Conn, {join, Channel}).

part(Conn, Channel) ->
	call(Conn, {part, Channel}).

quit(Conn, QuitMsg) ->
	call(Conn, {quit, QuitMsg}).

me(Conn, Action) ->
	call(Conn, {me, Action}).

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
			connect(Host, Port, Options);
		{error, _} ->
			timer:sleep(RetryTimeout),
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
	{Event, NewState} = parse_line(Data, State),
	notify(NewState, Event),
	{noreply, NewState, NewState#state.ping}.

notify(_State, noevent) ->
	noevent;
notify(State, Event) ->
	gen_event:notify(State#state.eventmgr, Event).

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
do_command({part, State, Channel}) ->
	send(State, "PART " ++ Channel);
do_command({quit, State, QuitMsg}) ->
	send(State, "QUIT " ++ QuitMsg);
do_command({me, State, Action}) ->
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

parse_line(Line, State) ->
	case regexp:first_match(Line, "^\s*:?[^:]+:") of
		%% TODO: part (:Spender_CGB!~spender_c@81.200.112.130 PART &pron)
		%% TODO: \s* is allowed but than `:' won't be stripped
		nomatch ->
			Header = string:strip(Line, both, $:), 
			Headers = string:tokens(Header, " "),
			parse_user(State, Headers);			
		{match, From, Len} ->
			Header = string:strip(string:substr(Line, From, Len), both, $:), 
			Headers = string:tokens(Header, " "),
			Text = string:substr(Line, From + Len, string:len(Line) - Len - 2),
			parse_user(State, Headers ++ [Text])
	end.

parse_user([MaybeLogin | Rest], State) ->
	case gregexp:groups(MaybeLogin, "\\(.*\\)!\\(.*\\)@\\(.*\\)") of
		{match, [Nick, Ident, Host]} ->
			User = {Nick, Ident, Host},
			parse_tokens([User | Rest], State);
		nomatch ->
			parse_tokens([MaybeLogin | Rest], State)
	end.	

parse_tokens([User, "PRIVMSG", Nick, Msg], State) when Nick == State#state.nick ->
	event({privmsg, User, Msg}, State);
parse_tokens([User, "PRIVMSG", Channel, Msg], State) ->
	parse_chanmsg(Channel, User, Msg, State);
parse_tokens([User, "TOPIC", Channel, Topic], State) ->
	event({topic, Channel, User, Topic}, State);
parse_tokens([User, "NICK", NewNick], State) ->
	event({nick, User, NewNick}, State);
parse_tokens([User, "JOIN", Channel], State) ->
	event({join, User, Channel}, State);
parse_tokens([User, "PART", Channel, Reason], State) ->
	event({part, User, Channel, Reason}, State);
parse_tokens([User, "QUIT", Reason], State) ->
	event({quit, User, Reason}, State);
parse_tokens([User, "KICK", Channel, Nick, Reason], State) when Nick == State#state.nick ->
	event({kicked, User, Channel, Reason}, State);
parse_tokens([User, "KICK", Channel, Nick, Reason], State) ->
	event({kick, User, Channel, Nick, Reason}, State);
parse_tokens([_Server, "376" | _], State) ->
	event({end_of_motd}, State);
parse_tokens([_Server, "332", _, Channel, Topic], State) ->
	event({chantopic, Channel, Topic}, State);
parse_tokens([_Server, "366", _, Channel, _], State) ->
	event({end_of_names, Channel}, State);
parse_tokens(["PING" | Server], State) ->
	pong(Server, State);
parse_tokens(Tokens, State) ->
	event({unknown, Tokens}, State).

parse_chanmsg(Channel, User, [1, $A, $C, $T, $I, $O, $N, $\ | Action], State) ->
	event({me, Channel, User, string:strip(Action, right, 1)}, State);
parse_chanmsg(Channel, User, Msg, State) ->
	event({chanmsg, Channel, User, Msg}, State).

pong(Server, State) ->
	{noevent, send("PONG " ++ Server ++ "\r\n", State)}.

event(Event, State) ->
	{{irc_event, Event}, State}.

%% utility

keyfind(Key, List, Default) ->
	case lists:keyfind(Key, 1, List) of
		false ->
			Default;
		{Key, Value} ->
			Value
	end.

set_data(Key, Data, State) ->
	State#state{data = lists:keystore(Key, 1, State#state.data, {Key, Data})}.

get_data(Key, State, Default) ->
	keyfind(Key, State#state.data, Default).
