-module(util).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-export([split/1, set_flag/2, unset_flag/2]).
-export([epoch/0, epoch/1]).
-export([uri_encode/1]).
-export([execvp/4, find_prog/2, signame/1]).
-export([check_latin/1, count_latin/1, check_letter/1, count_letters/1]).

split(String) ->
	re:split(String, "\s+", [{return, list}, trim]).

set_flag(Flag, Flags) ->
	[Flag | unset_flag(Flag, Flags)].

unset_flag(Flag, Flags) ->
	lists:delete(Flag, Flags).

epoch() -> epoch(sec).

epoch(sec) ->
	case erlang:now() of {M, S, _} -> M*1000000 + S end;
epoch(msec) ->
	case erlang:now() of {M, S, U} -> (M*1000000 + S)*1000 + U div 1000 end;
epoch(usec) ->
	case erlang:now() of {M, S, U} -> (M*1000000 + S)*1000000 + U end.

find_prog(File, nodir) ->
	os:find_executable(File);
find_prog(File, Dir) ->
	os:find_executable(File, Dir ++ ":" ++ os:getenv("PATH")).

-define(EXEC_LINE, 1048576).

%% Exec program (absolute path or relative to WD) with specified WD and Input.
execvp(File, Args, Dir, Input) ->
	case filename:pathtype(File) of
		absolute ->
			execvp0(File, [File | Args], Dir, Input);
		_ ->
			execvp0(filename:absname(File, Dir), [File | Args], Dir, Input)
	end.

%% Note that Args are expected to be utf8 while Arg0 is not.
execvp0(Path, [Arg0 | Args], Dir, Input) ->
	UtfArgs = [binary_to_list(utf8:encode(S)) || S <- Args],
	Port = open_port({spawn_executable, Path}, [{arg0, Arg0}, {args, UtfArgs}, {cd, Dir}, 
												{line, ?EXEC_LINE}, eof, exit_status, binary]),
	send_input(Port, Input),
	loop_port(Port, []).

send_input(Port, Input) when byte_size(Input) > 0 ->
	port_command(Port, Input);	
send_input(Port, Input) when length(Input) > 0 ->
	port_command(Port, utf8:encode(Input));
send_input(_, _) ->
	true.

-define(STATUS_WAIT, 5000).

loop_port(Port, Data) ->
	receive
		{Port, {data, {Eol, Line}}} ->
			loop_port(Port, [Eol, utf8:decode(Line) | Data]);
		{Port, eof} ->
			wait_port(Port, Data)
	end.

wait_port(Port, Data) ->
	ExitStatus = receive 
					 {Port, {exit_status, Status}} ->
						 transform_status(Status)
				 after ?STATUS_WAIT ->
						 timedout
				 end,
	{ExitStatus, transform_output(Data)}.

transform_output([_ | T]) ->
	lists:flatten(transform_output(T, []));
transform_output([]) ->
	[].

transform_output([eol | T], Lines) ->
	transform_output(T, [$\n | Lines]);
transform_output([noeol | T], Lines) ->
	transform_output(T, Lines);
transform_output([Line | T], Lines) ->	
	transform_output(T, [Line | Lines]);
transform_output([], Lines) ->
	Lines.

transform_status(0) ->
	success;
transform_status(2) ->
	{failure, einval};
transform_status(126) ->
	{failure, enoexec};
transform_status(127) ->
	{failure, enoent};
transform_status(I) when I > 128 ->
	{failure, signame(I - 128)};
transform_status(I) ->
	{failure, I}.

%% These are actually x86/ppc-specific.
signame(1) -> hup;
signame(2) -> int;
signame(3) -> quit;
signame(4) -> ill;
signame(5) -> trap;
signame(6) -> abrt;
signame(7) -> bus;
signame(8) -> fpe;
signame(9) -> kill;
signame(10) -> usr1;
signame(11) -> segv;
signame(12) -> usr2;
signame(13) -> pipe;
signame(14) -> alrm;
signame(15) -> term;
signame(16) -> stkflt;
signame(17) -> chld;
signame(18) -> cont;
signame(19) -> stop;
signame(20) -> tstp;
signame(21) -> ttin;
signame(22) -> ttou;
signame(23) -> urg;
signame(24) -> xcpu;
signame(25) -> xfsz;
signame(26) -> vtalrm;
signame(27) -> prof;
signame(28) -> winch;
signame(29) -> io;
signame(30) -> pwr;
signame(31) -> unused;
signame(I) when I < 64 -> {rt, I - 32};
signame(I) -> I.

uri_encode(Atom) when is_atom(Atom) ->
    uri_encode(atom_to_list(Atom));
uri_encode(Int) when is_integer(Int) ->
    uri_encode(integer_to_list(Int));
uri_encode(String) ->
    uri_encode(String, []).

uri_encode([], Acc) ->
    lists:reverse(Acc);
uri_encode([C | Rest], Acc) when (C > 32) and (C < 128) ->
    uri_encode(Rest, [C | Acc]);
uri_encode([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    uri_encode(Rest, [hexdigit(Lo), hexdigit(Hi), $% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%% Life sucks, doesn't it?..
check_latin(C) when C >= $a, C =< $z; C >= $A, C =< $Z; 
					C =:= $`; C =:= $[; C =:= $]; C =:= ${; C =:= $}; C =:= $:; C =:= $;; 
					C =:= $'; C =:= $"; C =:= $,; C =:= $<; C =:= $.; C =:= $>; C =:= $/; C =:= $? ->
	1;
check_latin(_) ->
	0.

check_letter(C) when C == $\ ; C >= $0, C =< $9; C == $!; C == $!; C == $%; C == $^; 
					 C == $*; C == $(; C == $); C == $_; C == $-; C == $=; C == $+ ->
	0;
check_letter(_) ->
	1.

count_latin(Str) ->
	lists:foldl(fun(C, Count) -> Count + check_latin(C) end, 0, Str).

count_letters(Str) ->
	lists:foldl(fun(C, Count) -> Count + check_letter(C) end, 0, Str).
