-module(irc_proto).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% public interface
-export([start_link/3, start/3, start_link/2, start/2, irc_command/2]).

%% @type irc_event() = {Type, Originator} | {Type, Originator, Arg1} | {Type, Originator, Arg1, Arg2} | etc
%%              Type = atom()
%%        Originator = list() | 'undefined'.
%% Type is atom describing event type (chanmsg, topic, etc), Originator is either channel name (when event belongs to channel), 
%% user name (when event is associated with some user, like quitting, changing nick, privmsg) or 
%% `undefined' if event is a server event.

-record(conf, {ping_timeout  = 60000,
			   sock_timeout  = 30000,
			   port          = 6667,
			   maxsend       = 400}).

-record(state, {owner      :: pid(),
				sock       :: port(),
				ping       :: integer(),
				maxsend    :: integer(),
				conf       :: #conf{},
			   	data = []  :: [tuple()]}).      %% misc data for further extensions

-define(CRLF, "\r\n").
-define(IS_DIGIT(Var), $0 =< Var, Var =< $9).

-compile([bin_opt_info]).

%% public interface

start_link(Host, Options) ->
	gen_server:start_link(?MODULE, {self(), Host, Options}, []).
start_link(ServerName, Host, Options) ->
	gen_server:start_link(ServerName, ?MODULE, {self(), Host, Options}, []).

start(Host, Options) ->
	gen_server:start(?MODULE, {self(), Host, Options}, []).
start(ServerName, Host, Options) ->
	gen_server:start(ServerName, ?MODULE, {self(), Host, Options}, []).

irc_command(IrcRef, Cmd) ->
	gen_server:cast(IrcRef, Cmd).

%% gen_server callbacks

%% real connect is deferred to handle_info
init({Owner, Host, Options}) ->
	process_flag(trap_exit, true),
	self() ! {connect, Host, conf(Options), Owner},
	{ok, undefined}.

conf(Options) ->
	conf(#conf{}, Options).

conf(Conf, [Option | Options]) ->
	NewConf = case Option of
		{ping_timeout, PingTimeout} ->
			Conf#conf{ping_timeout = PingTimeout};
		{sock_timeout, SockTimeout} ->
			Conf#conf{sock_timeout = SockTimeout};
		{port, Port} ->
			Conf#conf{port = Port};
		{maxsend, MaxSend} ->
			Conf#conf{maxsend = MaxSend};
		_ ->
			Conf
	end,
	conf(NewConf, Options);
conf(Conf, []) ->
	Conf.

handle_call(_Req, _From, State) ->
	{reply, nosuchcall, State, State#state.ping}.

handle_cast(Req, State) ->
	{noreply, do_command(Req, State), State#state.ping}.

%% EXIT signal is not handled and will terminate
handle_info(timeout, State) ->
	{stop, timeout, State};
handle_info({tcp_closed, Sock}, State) when Sock == State#state.sock ->
	{stop, tcp_closed, State};
handle_info({tcp_error, Sock, Reason}, State) when Sock == State#state.sock ->
	{stop, {tcp_error, Reason}, State};
handle_info({tcp, Sock, Data}, State) when Sock == State#state.sock ->
	{Event, NewState} = parse_line(utf8:decode(Data), State),
	{noreply, notify(Event, NewState), NewState#state.ping};
handle_info({connect, Host, Conf, Owner}, undefined) ->
	{ok, State} = connect(Host, Conf),
	{noreply, State#state{owner = Owner}, State#state.ping}.

notify(noevent, State) ->
	State;
notify(Event, #state{owner = Pid} = State) ->
	Pid ! {irc, self(), Event},
	State.

connect(Host, Conf) ->
	io:format("Connecting to ~p, conf ~p~n", [Host, Conf]),
	case gen_tcp:connect(Host, Conf#conf.port, 
						 [binary,
						  {packet, line},
						  {send_timeout, Conf#conf.sock_timeout},
						  {send_timeout_close, true}], Conf#conf.sock_timeout) of
		{ok, Sock} ->
			{ok, #state{sock = Sock, ping = Conf#conf.ping_timeout, maxsend = Conf#conf.maxsend, conf = Conf}};
		{error, timeout} ->
			io:format("Timeout~n", []),
			connect(Host, Conf);
		{error, Error} ->
			io:format("Error: ~p~n", [Error]),
			{error, Error}
	end.

%% Actually, this code and EXIT trapping are not neccessary as TCP socket is closed when controlling process dies

terminate(Reason, #state{sock = Sock}) ->
	io:format("OOPS: ~p (~p) terminating with reason ~p~n", [?MODULE, self(), Reason]),
	gen_tcp:close(Sock).

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
do_command({action, Channel, Action}, State) ->
	send(State, "PRIVMSG " ++ Channel ++ " :" ++ [1] ++ "ACTION ", Action, [1]);
do_command({mode, Channel, User, Mode}, State) ->
	send(State, "MODE " ++ Channel ++ " " ++ Mode ++ " " ++ User);
do_command({umode, User, Mode}, State) ->
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
	send(State, "TOPIC " ++ Channel ++ " :" ++ Topic);
do_command({pong, Server}, State) ->
	send(State, "PONG :" ++ Server).

send(State, Prefix) ->
	send(State, Prefix, [], []).

send(State, Prefix, Line) ->
	send(State, Prefix, Line, []).

send(State, Prefix, Line, Suffix) ->
	send_bytes(State, utf8:encode(Prefix), utf8:encode(Line), utf8:encode(Suffix)).

send_bytes(State, Prefix, Bytes, Suffix) when size(Bytes) =< State#state.maxsend ->
	true = sanity_check(Bytes),
	ok = gen_tcp:send(State#state.sock, <<Prefix/binary, Bytes/binary, Suffix/binary, ?CRLF>>),
	State;
send_bytes(State, Prefix, Bytes, Suffix) ->
	{ByteLine, Rest} = utf8:split(Bytes, State#state.maxsend),
	S = send_bytes(State, Prefix, ByteLine, Suffix),
	send_bytes(S, Prefix, Rest, Suffix).

%% Ensure buffer has no CRLF and null bytes
sanity_check(<<C/utf8, Rest/binary>>) ->
	case C of
		$\r -> false;
		$\n -> false;
		0   -> false;
		_   ->
			sanity_check(Rest)
	end;
sanity_check(<<>>) ->
	true.

%% IRC protocol parser

chop(Line) ->
	string:left(Line, string:len(Line) - 2).

parse_line(Line, State) ->
	%% @warn Doesn't parse ISUPPORT correctly
	case re:run(Line, "^:?([^:]+):(.*)" ++ ?CRLF, [unicode, {capture, all_but_first, list}]) of
		nomatch ->
			Header = string:strip(chop(Line), left, $:),
			Headers = string:tokens(Header, " "),
			parse_special(Headers, State);
		{match, [Header, Text]} ->
			Headers = string:tokens(Header, " "),
			parse_special(Headers ++ [Text], State)
	end.

parse_special(["ERROR", Error], State) ->
	event({error, undefined, Error}, State);
parse_special(["PING", Server], State) ->
	event({ping, undefined, Server}, State);
parse_special(Tokens, State) ->
	parse_user(Tokens, State).

parse_user([MaybeLogin | Rest], State) ->
	case re:run(MaybeLogin, "(.*)!(.*)@(.*)", [unicode, {capture, all_but_first, list}]) of
		{match, [Nick, Ident, Host]} ->
			User = {Nick, Ident, Host},
			parse_code([User | Rest], State);
		nomatch ->
			parse_code([MaybeLogin | Rest], State)
	end.

parse_code([Target, [D1, D2, D3] | Rest], State) when ?IS_DIGIT(D1), ?IS_DIGIT(D2), ?IS_DIGIT(D3) ->
	parse_tokens([Target, irc_codes:code_to_atom([D1, D2, D3]) | Rest], State);
parse_code(Tokens, State) ->
	parse_tokens(Tokens, State).

parse_tokens([User, "PRIVMSG", Target, Msg], State) ->
	parse_privmsg(Target, User, Msg, State);
parse_tokens([User, "TOPIC", Channel, Topic], State) ->
	event({topic, Channel, User, Topic}, State);
parse_tokens([User, "NICK", NewNick], State) ->
	event({nick, NewNick, User}, State);
parse_tokens([User, "JOIN", Channel], State) ->
	event({join, Channel, User}, State);
parse_tokens([User, "PART", Channel, Reason], State) ->
	event({part, Channel, User, Reason}, State);
parse_tokens([User, "PART", Channel], State) ->
	event({part, Channel, User, []}, State);
parse_tokens([User, "QUIT", Reason], State) ->
	event({quit, User, Reason}, State);
parse_tokens([User, "KICK", Channel, Nick, Reason], State) ->
	event({kick, Channel, User, Nick, Reason}, State);
parse_tokens([Nick, "MODE", Nick, Mode], State) ->
	event({umode, Nick, Mode}, State);
parse_tokens([User, "MODE", Channel, Mode, Nick], State) ->
	event({mode, Channel, User, Mode, Nick}, State);
parse_tokens([_Server, "NOTICE", _Target, Notice], State) ->
	event({notice, undefined, Notice}, State);
parse_tokens([_Server, topic, _Target, Channel, Topic], State) ->
	event({chantopic, Channel, Topic}, State);
parse_tokens([_Server, topicinfo, _Target, Channel, Author, Ts], State) ->
	event({chantopic, Channel, Author, list_to_integer(Ts)}, State);
parse_tokens([_Server, namreply, _Target, _, Channel, Users], State) ->
	event({names, Channel, parse_names(Users)}, State);
parse_tokens([_Server, endofnames, _Target, Channel, Text], State) ->
	event({endofnames, Channel, Text}, State);
parse_tokens([_Server, myinfo, _Target, Server, Vsn, Umodes, Chanmodes], State) ->
	event({myinfo, undefined, Server, Vsn, Umodes, Chanmodes}, State);
parse_tokens([_Server, nicknameinuse, _Target, Nick, Text], State) ->
	event({nicknameinuse, Nick, Text}, State);
parse_tokens([_Server, Reply, _Target, Channel, Text], State) 
  when Reply =:= inviteonlychan; Reply =:= bannedfromchan; Reply =:= badchannelkey; Reply =:= badchanmask; 
	   Reply =:= nosuchchannel; Reply =:= channelisfull; Reply =:= toomanychannels; Reply =:= killdeny ->
	event({Reply, Channel, Text}, State);
parse_tokens([_Server, Reply, _Target, Text], State) when is_atom(Reply) ->
	event({Reply, undefined, Text}, State);
parse_tokens(Tokens, State) ->
	event({unknown, undefined, Tokens}, State).

parse_privmsg(Channel, User, [1] ++ "ACTION " ++ Action, State) ->
	event({action, Channel, User, string:strip(Action, right, 1)}, State);
parse_privmsg(Target, User, Msg, State) ->
	%% @notice Target may also be own nick (which means privmsg) or channel (chanmsg) but that's detected 
	%%         on higher level since we don't know our nick here
	event({privmsg, Target, User, Msg}, State).

parse_names(Names) ->
	[parse_name(Token) || Token <- string:tokens(Names, " ")].

parse_name([$* | Name]) ->
	{op, Name, [owner]};
parse_name([$@ | Name]) ->
	{op, Name, []};
parse_name([$+ | Name]) ->
	{user, Name, [voice]};
parse_name(Name) ->
	{user, Name, []}.

event(Event, State) ->
	{Event, State}.

%% keyfind(Key, List, Default) ->
%% 	case lists:keysearch(Key, 1, List) of
%% 		false ->
%% 			Default;
%% 		{value, {Key, Value}} ->
%% 			Value
%% 	end.

%% set_data(Key, Data, State) ->
%% 	State#state{data = lists:keystore(Key, 1, State#state.data, {Key, Data})}.

%% get_data(Key, State, Default) ->
%% 	keyfind(Key, State#state.data, Default).
