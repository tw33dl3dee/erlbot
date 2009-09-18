-module(irc_conn).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([behaviour_info/1]).

%% public interface
-export([start_link/4, start/4]).
-export([chanmsg/3, privmsg/3, join/2, part/2, quit/2, me/2, 
		 mode/4, mode/3, user/3, nick/2, oper/3, kick/4, topic/3]).

-include("irc_conn.hrl").

-record(state, {module    :: atom(),
				module_state,
				nick      :: list(),
				channels  :: [list()],
				sock      :: port(),
				ping      :: integer(),
			   	data = [] :: [tuple()]}).

%% public interface

%%%% Testing 
-export([test/0, handle_irc/1]).

test() ->
	irc_conn:start({local, irc}, ?MODULE, "192.168.1.1", []),
	irc_conn:nick(irc, "test"),
	irc_conn:user(irc, "test", "test"),
	irc_conn:join(irc, "#pron"),
	irc_conn:chanmsg(irc, "#pron", "hi all").

handle_irc(Event) ->
	io:format("Event: ~p~n", [Event]).

%%%%

behaviour_info(callbacks) ->
    [{init,1},{handle_irc,1},
     {terminate,2},{code_change,3}];
behaviour_info(_) ->
    undefined.

start_link(undef, Module, Host, Options) ->
	gen_server:start_link(?MODULE, {Module, Host, Options}, []);
start_link(ServerName, Module, Host, Options) ->
	gen_server:start_link(ServerName, ?MODULE, {Module, Host, Options}, []).

start(undef, Module, Host, Options) ->
	gen_server:start(?MODULE, {Module, Host, Options}, []);
start(ServerName, Module, Host, Options) ->
	gen_server:start(ServerName, ?MODULE, {Module, Host, Options}, []).

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
	gen_server:call(Conn, Req, infinity).

%% gen_server callbacks

init({Module, Host, Options}) ->
	State = connect(Host, Options),
	{ok, State#state{module = Module}, State#state.ping}.

connect(Host, Options) ->
	SockTimeout = keyfind(sock_timeout, Options, ?SOCK_TIMEOUT),
	RetryTimeout = keyfind(retry_timeout, Options, ?RETRY_TIMEOUT),
	PingTimeout = keyfind(ping_timeout, Options, ?PING_TIMEOUT),
	Port = keyfind(port, Options, ?IRC_PORT),
	io:format("Connecting ~p:~p ~p ~p ~p~n", [Host, Port, SockTimeout, RetryTimeout, PingTimeout]),
	case gen_tcp:connect(Host, Port, [binary,
									  {packet, line},
									  {send_timeout, SockTimeout}
									  {send_timeout_close, true}
									 ], 
						 SockTimeout) of
		{ok, Sock} ->
			io:format("ok~n", []),
			#state{sock = Sock, ping = PingTimeout};
		{error, timeout} ->
			io:format("timeout~n", []),
			connect(Host, Options);
		{error, _} ->
			io:format("err~n", []),
			timer:sleep(RetryTimeout),
			connect(Host, Options);
		Any ->
			io:format(":~p~n", [Any])
	end.

handle_call(Req, _From, State) ->
	{reply, ok, do_command(Req, State), State#state.ping}.

handle_cast(_Req, State) ->
	{noreply, State, State#state.ping}.

handle_info(timeout, State) ->
	{stop, timeout, State};
handle_info({tcp_closed, Sock}, State) when Sock == State#state.sock ->
	{stop, tcp_closed, State};
handle_info({tcp_error, Sock, Reason}, State) when Sock == State#state.sock ->
	{stop, {tcp_error, Reason}, State};
handle_info({tcp, Sock, Data}, State) when Sock == State#state.sock ->
	{Event, NewState} = parse_line(utf8:decode(Data), State),
	notify(NewState, Event),
	{noreply, NewState, NewState#state.ping}.

notify(_State, noevent) ->
	noevent;
notify(#state{module = M}, Event) ->
	M:handle_irc(Event).
	%%gen_event:notify(State#state.eventmgr, Event).

terminate(_Reason, State) ->
	gen_tcp:close(State#state.sock).

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

%% IRC protocol commands

do_command({chanmsg, Channel, Msg}, State) ->
	do_command({privmsg, Channel, Msg}, State);
do_command({privmsg, To, Msg}, State) ->
	send(State, "PRIVMSG " ++ To ++ " :", Msg);
do_command({join, Channel}, State) ->
	send(State, "JOIN :" ++ Channel);
do_command({part, Channel}, State) ->
	send(State, "PART " ++ Channel);
do_command({quit, QuitMsg}, State) ->
	send(State, "QUIT :" ++ QuitMsg);
do_command({me, Channel, Action}, State) ->
	send(State, "PRIVMSG " ++ Channel ++ " :" ++ [1] ++ "ACTION ", Action, [1]);
do_command({mode, Channel, User, Mode}, State) ->
	send(State, "MODE " ++ Channel ++ " " ++ Mode ++ " " ++ User);
do_command({mode, User, Mode}, State) ->
	send(State, "MODE " ++ User ++ " " ++ Mode);
do_command({user, Login, LongName}, State) ->
	send(State, "USER " ++ Login ++ " 8 * :" ++ LongName);
do_command({nick, Nick}, State) ->
	send(State, "NICK " ++ Nick);
do_command({oper, Login, Passwd}, State) ->
	send(State, "OPER " ++ Login ++ " " ++ Passwd);
%% do_command({login, Nick, Login, LongName}, State) ->
%% 	try_nick(State, Nick),
%% 	user(State, Login, LongName);
%% do_command({login, Nick, Login, Passwd, LongName}, State) ->
%% 	try_nicn(State, Nick),
%% 	user(State, Login, LongName),
%% 	oper(State, Login, Passwd);
%% do_command({login, Nick, Login, Passwd, LongName}, State) ->
%% 	try_nick, Nick),
%% 	user(State, Login, LongName),
%% 	oper(State, Login, Passwd),
%% 	nick(State, Nick);
do_command({kick, Channel, Nick, Reason}, State) ->
	send(State, "KICK " ++ Channel ++ " " ++ Nick ++ " :" ++ Reason);
do_command({topic, Channel, Topic}, State) ->
	send(State, "TOPIC " ++ Channel ++ " :" ++ Topic).

send(State, Prefix) ->
	send(State, Prefix, [], []).

send(State, Prefix, Data) ->
	send(State, Prefix, Data, []).

send(State, Prefix, Data, Suffix) ->
	send_bytes(State, utf8:encode(Prefix), utf8:encode(Data), utf8:encode(Suffix)).

send_bytes(State, Prefix, Data, Suffix) when size(Data) =< ?MAXSEND ->
	ok = gen_tcp:send(State#state.sock, <<Prefix/binary, Data/binary, Suffix/binary, ?CRLF>>),
	State;
send_bytes(State, Prefix, Data, Suffix) ->
	{Line, Rest} = utf8:split(Data, ?MAXSEND),
	send_bytes(State, Prefix, Line, Suffix),
	send_bytes(State, Prefix, Rest, Suffix).

%% IRC protocol parser

parse_line(Line, State) ->
	%% TODO: move to `re'
	case regexp:first_match(Line, "^:?[^:]+:") of
		nomatch ->
			Header = string:strip(string:left(Line, string:len(Line) - 2), left, $:),
			Headers = string:tokens(Header, " "),
			parse_user(Headers, State);
		{match, From, Len} ->
			Header = string:strip(string:substr(Line, From, Len), both, $:),
			Headers = string:tokens(Header, " "),
			Text = string:substr(Line, From + Len, string:len(Line) - Len - 2),
			parse_user(Headers ++ [Text], State)
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
parse_tokens([User, "PART", Channel], State) ->
	event({part, User, Channel, []}, State);
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
parse_tokens([_Server, "333", _, Channel, TopicAuthor, TopicTime], State) ->
	event({chantopic, Channel, TopicAuthor, list_to_integer(TopicTime)}, State);
parse_tokens([_Server, "353", _, _, Channel, Users], State) ->
	event({names, Channel, parse_names(Users)}, State);
parse_tokens([_Server, "366", _, Channel, _], State) ->
	event({joined, Channel}, State);
parse_tokens(["PING" | Server], State) ->
	pong(Server, State);
parse_tokens(Tokens, State) ->
	event({unknown, Tokens}, State).

parse_chanmsg(Channel, User, [1, $A, $C, $T, $I, $O, $N, $\ | Action], State) ->
	event({me, Channel, User, string:strip(Action, right, 1)}, State);
parse_chanmsg(Channel, User, Msg, State) ->
	event({chanmsg, Channel, User, Msg}, State).

parse_names(Names) ->
	lists:map(fun parse_name/1, string:tokens(Names, " ")).

parse_name([$* | Name]) ->
	{op, Name, [owner]};
parse_name([$@ | Name]) ->
	{op, Name, []};
parse_name([$+ | Name]) ->
	{user, Name, [voice]};
parse_name(Name) ->
	{user, Name, []}.

pong(Server, State) ->
	{noevent, send(State, "PONG " ++ Server ++ "\r\n")}.

event(Event, State) ->
	{{irc_event, Event}, State}.

%% utility

keyfind(Key, List, Default) ->
	case lists:keysearch(Key, 1, List) of
		false ->
			Default;
		{value, {Key, Value}} ->
			Value
	end.

set_data(Key, Data, State) ->
	State#state{data = lists:keystore(Key, 1, State#state.data, {Key, Data})}.

get_data(Key, State, Default) ->
	keyfind(Key, State#state.data, Default).
