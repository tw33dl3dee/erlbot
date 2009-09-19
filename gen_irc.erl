-module(gen_irc).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([behaviour_info/1]).

%% public interface
-export([start_link/5, start/5]).
-export([chanmsg/3, privmsg/3, join/2, part/2, quit/2, action/2, 
		 mode/4, mode/3, user/3, nick/2, oper/3, kick/4, topic/3]).

-record(state, {mod        :: atom(),
				mod_state  :: term(),
				nick        :: list(),
				sock          :: port(),
				ping          :: integer(),
			   	data = []     :: [tuple()]}).      %% misc data for further extensions

-record(conf, {ping_timeout  = 60000,
			   sock_timeout  = 30000,
			   retry_timeout = 30000,
			   port          = 6667,
			   maxsend       = 400}).

-define(?CRLF).

%% public interface

behaviour_info(callbacks) ->
    [{init,1}, {handle_irc_event,2},
     {terminate,2}, {code_change,3}];
behaviour_info(_) ->
    undefined.

start_link(undef, Module, ModArgs, Host, Options) ->
	gen_server:start_link(?MODULE, {Module, ModArgs, Host, Options}, []);
start_link(ServerName, Module, ModArgs, Host, Options) ->
	gen_server:start_link(ServerName, ?MODULE, {Module, ModArgs, Host, Options}, []).

start(undef, Module, ModArgs, Host, Options) ->
	gen_server:start(?MODULE, {Module, ModArgs, Host, Options}, []);
start(ServerName, Module, ModArgs, Host, Options) ->
	gen_server:start(ServerName, ?MODULE, {Module, ModArgs, Host, Options}, []).

chanmsg(Conn, Channel, Msg) ->
	cast(Conn, {chanmsg, Channel, Msg}).

privmsg(Conn, To, Msg) ->
	cast(Conn, {privmsg, To, Msg}).

join(Conn, Channel) ->
	cast(Conn, {join, Channel}).

part(Conn, Channel) ->
	cast(Conn, {part, Channel}).

quit(Conn, QuitMsg) ->
	cast(Conn, {quit, QuitMsg}).

action(Conn, Action) ->
	cast(Conn, {action, Action}).

mode(Conn, Channel, User, Mode) ->
	cast(Conn, {mode, Channel, User, Mode}).

mode(Conn, User, Mode) ->
	cast(Conn, {mode, User, Mode}).

user(Conn, Login, LongName) ->
	cast(Conn, {user, Login, LongName}).

nick(Conn, Nick) ->
	cast(Conn, {nick, Nick}).

oper(Conn, Login, Passwd) ->
	cast(Conn, {oper, Login, Passwd}).

kick(Conn, Channel, Nick, Reason) ->
	cast(Conn, {kick, Channel, Nick, Reason}).

topic(Conn, Channel, Topic) ->
	cast(Conn, {topic, Channel, Topic}).

channels(Conn) ->
	cast(Conn, channels).

cast(Conn, Req) ->
	gen_server:cast(Conn, Req).

%% gen_server callbacks

init({Module, ModArgs, Host, Options}) ->
	State = connect(Host, conf(Options)),
	{ok, State#state{mod = Module, mod_state = Module:init(ModArgs)}, State#state.ping}.

conf(Conf, [Option | Options]) ->
	NewConf = case Option of
		{ping_timeout, PingTimeout} ->
			Conf#conf{ping_timeout = PingTimeout};
		{sock_timeout, SockTimeout} ->
			Conf#conf{sock_timeout = SockTImeout};
		{retry_timeout, RetryTimeout} ->
			Conf#conf{retry_timeout = RetryTimeout};
		{port, Port} ->
			Conf#conf{port = Port};
		{maxsend, MaxSend} ->
			Conf#conf{maxsend = MaxSend}
	end,
	conf(NewConf, Options);
conf(Conf, []) ->
	Conf.

connect(Host, Conf) ->
	case gen_tcp:connect(Host, Conf#conf.port, 
						 [binary,
						  {packet, line},
						  {send_timeout, Conf#conf.sock_timeout},
						  {send_timeout_close, true}], Conf#conf.sock_timeout) of
		{ok, Sock} ->
			io:format("ok~n", []),
			#state{sock = Sock, ping = Conf#conf.ping_timeout};
		{error, timeout} ->
			io:format("timeout~n", []),
			connect(Host, Conf);
		{error, _} ->
			io:format("err~n", []),
			timer:sleep(Conf#conf.retry_timeout),
			connect(Host, Conf);
		Any ->
			io:format(":~p~n", [Any])
	end.

handle_call(_Req, _From, State) ->
	{reply, nosuchcall, State, State#state.ping}.

handle_cast(Req, State) ->
	{noreply, do_command(Req, State), State#state.ping}.

handle_info(timeout, State) ->
	{stop, timeout, State};
handle_info({tcp_closed, Sock}, State) when Sock == State#state.sock ->
	{stop, tcp_closed, State};
handle_info({tcp_error, Sock, Reason}, State) when Sock == State#state.sock ->
	{stop, {tcp_error, Reason}, State};
handle_info({tcp, Sock, Data}, State) when Sock == State#state.sock ->
	{Event, NewState} = parse_line(utf8:decode(Data), State),
	{noreply, notify(Event, NewState), NewState#state.ping}.

notify(noevent, State) ->
	State;
notify(Event, State) ->
	#state{mod = M, mod_state = S} = State,
	State#state{mod_state = M:handle_irc_event(Event, S)}.

terminate(Reason, State) ->
	#state{modu = M, mod_state = S} = State,
	M:terminate(Reason, S),
	gen_tcp:close(State#state.sock).

code_change(Vsn, State, Extra) ->
	#state{mod = M, mod_state = S} = State,
	{ok, State#state{mod_state = M:code_change(Vsn, S, Extra)}}.

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
do_command({action, Channel, Action}, State) ->
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
			parse_code([User | Rest], State);
		nomatch ->
			parse_code([MaybeLogin | Rest], State)
	end.

-define(IS_DIGIT(Var), $0 =< Var, Var =< $9).

parse_code([Target, [D1, D2, D3] | Rest], State) when ?IS_DIGIT(D1), ?IS_DIGIT(D2), ?IS_DIGIT(D3) ->
	parse_tokens([Target, irc_codes:code_to_atom([D1, D2, D3]) | Rest]);
parse_code(Tokens, State) ->
	parse_tokens(Tokens).

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
parse_tokens([_Server, endofmotd | _], State) ->
	event({end_of_motd}, State);
parse_tokens([_Server, topic, _, Channel, Topic], State) ->
	event({chantopic, Channel, Topic}, State);
parse_tokens([_Server, topicinfo, _, Channel, TopicAuthor, TopicTime], State) ->
	event({chantopic_info, Channel, TopicAuthor, list_to_integer(TopicTime)}, State);
parse_tokens([_Server, namreply, _, _, Channel, Users], State) ->
	event({names, Channel, parse_names(Users)}, State);
parse_tokens([_Server, endofnames, _, Channel, _], State) ->
	event({joined, Channel}, State);
parse_tokens(["PING" | Server], State) ->
	pong(Server, State);
parse_tokens(Tokens, State) ->
	event({unknown, Tokens}, State).

parse_chanmsg(Channel, User, [1, $A, $C, $T, $I, $O, $N, $\ | Action], State) ->
	event({action, Channel, User, string:strip(Action, right, 1)}, State);
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
	{Event, State}.
