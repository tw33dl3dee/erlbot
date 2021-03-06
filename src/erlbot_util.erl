%%%-------------------------------------------------------------------
%%% File    : erlbot_util.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_util).

-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% String utils
-export([multiline/1, multiline/2, split/1, split/2, contains/2, join/2]).
-export([lowercase/1, uppercase/1, switchcase/1]).
-export([lc/1, uc/1, sc/1]).
-export([words/2]).

%% Datetime utils
-export([epoch/0, epoch/1]).
-export([add_days/2, add_seconds/2, valid_datetime/1, time_diff/2, date_diff/2]).
-export([convert_time_abs/3, convert_time_rel/2, convert_time_rel_diff/2]).
-export([unix_timestamp/1, unix_timestamp/0, from_unix_timestamp/1]).
-export([timestamp_to_list/3, datetime_to_list/3, datetimes_to_lists/2]).

%% OS interface
-export([execv/3, execv/4, execvp/2, execvp/3, system/1, system/2]).
-export([find_prog/2, signame/1]).
-export([read_file/1]).

%% Misc
-export([set_flag/2, unset_flag/2]).
-export([maybe_int/1]).
-export([uri_encode/1]).

multiline(Term) ->
	string:tokens(lists:flatten(io_lib:print(Term)), io_lib:nl()).

multiline(Format, Data) when is_list(Data) ->
	string:tokens(lists:flatten(io_lib:format(Format, Data)), io_lib:nl());
multiline(Format, Data) ->
	multiline(Format, [Data]).

split(String) -> re:split(String, "\s+", [unicode, {return, list}, trim]).

split(String, Delim) -> re:split(String, Delim, [unicode, {return, list}, trim]).

-define(WORD_SEP, "[^\\pL\\pN]+").  % regexp describing non-word characters

%% Split String of words of minimum length MinLen
words(String, MinLen) -> 
	[W || W <- re:split(String, ?WORD_SEP, [unicode, {return, list}, trim]), length(W) >= MinLen].

contains(String, Pattern) ->
	case re:run(String, Pattern, [unicode, caseless, {capture, none}]) of
		match -> true;
		nomatch -> false
	end.

join(Sep, [H|T]) -> [H | [[Sep, El] || El <- T]];
join(_, []) -> [].

set_flag(Flag, Flags) -> [Flag | unset_flag(Flag, Flags)].

unset_flag(Flag, Flags) -> lists:delete(Flag, Flags).

maybe_int(Str) ->
	case catch list_to_integer(Str) of
		X when is_integer(X) -> X;
		_ -> Str
	end.

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

%% Exec shell command with given input.
system(Command, Input) ->
	UtfCmd = binary_to_list(utf8:encode(Command)),
	Port = open_port({spawn, UtfCmd}, [{line, ?EXEC_LINE}, exit_status, binary, stderr_to_stdout]),
	send_input(Port, Input),
	loop_port(Port, []).

system(Command) -> system(Command, <<>>).

%% Exec program (absolute path or relative to WD) with specified WD and Input.
execv(File, Args, Dir, Input) ->
	% If File is absolute, Dir will be ignored
	Path = filename:absname(filename:join(Dir, File)),
	execvp0(Path, [File | Args], Dir, Input).

execv(File, Args, Dir) -> execv(File, Args, Dir, <<>>).

execvp(Program, Args, Input) ->
	Path = find_prog(Program, nodir),
	{ok, Pwd} = file:get_cwd(),
	execvp0(Path, [Program | Args], Pwd, Input).	

execvp(Program, Args) -> execvp(Program, Args, <<>>).

%% Note that Args are expected to be utf8 while Arg0 is not.
execvp0(Path, [Arg0 | Args], Dir, Input) ->
	UtfArgs = [binary_to_list(utf8:encode(S)) || S <- Args],
	Port = open_port({spawn_executable, Path},
                     [{arg0, Arg0}, {args, UtfArgs}, {cd, Dir}, {line, ?EXEC_LINE},
                      exit_status, binary, stderr_to_stdout]),
	send_input(Port, Input),
	loop_port(Port, []).

send_input(Port, Input) when byte_size(Input) > 0 ->
	port_command(Port, Input);	
send_input(Port, Input) when length(Input) > 0 ->
	port_command(Port, utf8:encode(Input));
send_input(_, _) ->
	true.

loop_port(Port, Data) ->
	receive
		{Port, {data, {Eol, Line}}} ->
			loop_port(Port, [Eol, utf8:decode(Line) | Data]);
        {Port, {exit_status, Status}} ->
            {transform_status(Status), transform_output(Data)};
        {Port, closed} ->
            {success, transform_output(Data)}
	end.

transform_output(Data) ->
	transform_output(Data, []).

transform_output([eol, Line | T], Lines) ->
	transform_output(T, [Line | Lines]);
transform_output([noeol, Cont | T], [Begin | Lines]) ->
	transform_output(T, [Begin ++ Cont | Lines]);
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
signame(1) -> 'SIGHUP';
signame(2) -> 'SIGINT';
signame(3) -> 'SIGQUIT';
signame(4) -> 'SIGILL';
signame(5) -> 'SIGTRAP';
signame(6) -> 'SIGABRT';
signame(7) -> 'SIGBUS';
signame(8) -> 'SIGFPE';
signame(9) -> 'SIGKILL';
signame(10) -> 'SIGUSR1';
signame(11) -> 'SIGSEGV';
signame(12) -> 'SIGUSR2';
signame(13) -> 'SIGPIPE';
signame(14) -> 'SIGALRM';
signame(15) -> 'SIGTERM';
signame(16) -> 'SIGSTKFLT';
signame(17) -> 'SIGCHLD';
signame(18) -> 'SIGCONT';
signame(19) -> 'SIGSTOP';
signame(20) -> 'SIGTSTP';
signame(21) -> 'SIGTTIN';
signame(22) -> 'SIGTTOU';
signame(23) -> 'SIGURG';
signame(24) -> 'SIGXCPU';
signame(25) -> 'SIGXFSZ';
signame(26) -> 'SIGVTALRM';
signame(27) -> 'SIGPROF';
signame(28) -> 'SIGWINCH';
signame(29) -> 'SIGIO';
signame(30) -> 'SIGPWR';
signame(31) -> 'SIGUNUSED';
signame(I) when I < 64 -> {'SIGRT', I - 32};
signame(I) -> I.

uri_encode(Atom) when is_atom(Atom) ->
    uri_encode(atom_to_list(Atom));
uri_encode(Int) when is_integer(Int) ->
    uri_encode(integer_to_list(Int));
uri_encode(String) when is_list(String) ->
    uri_encode(String, []).

uri_encode([], Acc) ->
    lists:reverse(Acc);
uri_encode([C | Rest], Acc) when (C > 32) and (C < 128) ->
    uri_encode(Rest, [C | Acc]);
uri_encode([C | Rest], Acc) ->
    uri_encode(<<C/utf8>>, Rest, Acc).

uri_encode(<<Hi:4, Lo:4, Rest/binary>>, S, Acc) ->
	uri_encode(Rest, S, [hexdigit(Lo), hexdigit(Hi), $% | Acc]);
uri_encode(<<>>, S, Acc)  ->
	uri_encode(S, Acc).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

read_file(FileName) ->
	{ok, Io} = file:open(FileName, [read, raw, binary, read_ahead]),
	read_lines(Io).

read_lines(Io) ->
	read_lines(Io, file:read_line(Io), []).

read_lines(Io, {ok, Bin}, Lines) ->
	S = byte_size(Bin) - 1,
	case Bin of 
		<<Line:S/binary, $\n:8>> ->
			read_lines(Io, file:read_line(Io), [Line | Lines]);
		_ ->
			read_lines(Io, file:read_line(Io), [Bin | Lines])
	end;
read_lines(Io, eof, Lines) ->
	file:close(Io),
	{ok, lists:reverse(Lines)};
read_lines(Io, Error, _) ->
	file:close(Io),
	Error.

add_days(Date, Days) ->
	G = calendar:date_to_gregorian_days(Date),
	calendar:gregorian_days_to_date(G + Days).

add_seconds(DateTime, Seconds) ->
	G = calendar:datetime_to_gregorian_seconds(DateTime),
	calendar:gregorian_seconds_to_datetime(G + Seconds).

valid_datetime({Date, {H, M, S}}) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 ->
	calendar:valid_date(Date);
valid_datetime(_) ->
	false.

-define(UNIX_EPOCH, {{1970, 1, 1}, {0, 0, 0}}).

%% Erlang datetime to Unix timestamp
unix_timestamp({YMD, HMS, U}) ->
	unix_timestamp({YMD, HMS}) + U/1000000;
unix_timestamp({YMD, HMS}) ->
	calendar:datetime_to_gregorian_seconds({YMD, HMS}) - calendar:datetime_to_gregorian_seconds(?UNIX_EPOCH).

%% Returns current Unix timestamp as {number(), list()}
unix_timestamp() ->
	{M, S, Usec} = erlang:now(),
	Sec = M*1000000 + S,
	{Sec + Usec/1000000, 
	 io_lib:format("~b.~6..0b", [Sec, Usec])}.

%% Unix timestamp to Erlang datetime
from_unix_timestamp(Ts) when is_integer(Ts) ->
	calendar:gregorian_seconds_to_datetime(Ts + calendar:datetime_to_gregorian_seconds(?UNIX_EPOCH));
from_unix_timestamp(Ts) when is_float(Ts) ->
	{YMD, HMS} = from_unix_timestamp(trunc(Ts)),
	{YMD, HMS, trunc((Ts - trunc(Ts))*1000000)}.

timestamp_to_list(Tz, Format, Ts) ->
	datetime_to_list(Tz, Format, from_unix_timestamp(trunc(Ts))).

datetime_to_list(local, Format, DateTime) ->
	datetime_to_list(universal, Format, calendar:universal_time_to_local_time(DateTime));
datetime_to_list(universal, time, {_, {HH, MM, SS}}) ->
	io_lib:format("~2..0b:~2..0b:~2..0b", [HH, MM, SS]);
datetime_to_list(universal, date, {{Y, M, D}, _}) ->
	io_lib:format("~2..0b/~2..0b/~2..0b", [Y rem 1000, M, D]);
datetime_to_list(universal, datetime, DateTime) ->
	[datetime_to_list(universal, date, DateTime), 
	 " ", 
	 datetime_to_list(universal, time, DateTime)].

datetimes_to_lists(universal, DateTimes) ->
    datetimes_to_lists2(DateTimes, []);
datetimes_to_lists(local, DateTimes) ->
    datetimes_to_lists2([calendar:universal_time_to_local_time(DateTime)
                         || DateTime <- DateTimes], []).

datetimes_to_lists2([{Date, _} = DateTime | Rest], [{Date, _} | _] = Prev) ->
    [datetime_to_list(universal, time, DateTime)
     | datetimes_to_lists2(Rest, [DateTime | Prev])];
datetimes_to_lists2([DateTime | Rest], Prev) ->
    [datetime_to_list(universal, datetime, DateTime)
     | datetimes_to_lists2(Rest, [DateTime | Prev])];
datetimes_to_lists2([], _) -> [].

%% Difference in seconds between 2 datetimes
time_diff(DateTime1, DateTime2) ->
	calendar:datetime_to_gregorian_seconds(DateTime1) - calendar:datetime_to_gregorian_seconds(DateTime2).

%% Difference in days between 2 dates
date_diff(Date1, Date2) ->
	calendar:date_to_gregorian_days(Date1) - calendar:date_to_gregorian_days(Date2).

%% Relative time offset to seconds
convert_time_rel_diff(HH, MM) ->
	HH*3600 + MM*60.

%% Relative time offset to universal datetime
convert_time_rel(HH, MM) ->
	Diff = convert_time_rel_diff(HH, MM),
	{time, add_seconds(erlang:universaltime(), -Diff)}.

%% Absolutime time to universal datetime
%% Shift may be `yesterday'  (when time is in future, it's shifted one day back) 
%% or `tomorrow' (when time is in past, it's shifted one day back)
convert_time_abs(HH, MM, Shift) when HH >= 0, HH < 24, MM >= 0, MM < 60 ->
	convert_time(HH, MM, erlang:localtime(), Shift);
convert_time_abs(_, _, _) ->
	undefined.

%% Requested HMS is within today
convert_time(HH, MM, {YMD, {HH1, MM1, _}}, yesterday) when {HH, MM} < {HH1, MM1} ->
	convert_time({YMD, {HH, MM, 0}});
convert_time(HH, MM, {YMD, {HH1, MM1, _}}, tomorrow) when {HH, MM} > {HH1, MM1} ->
	convert_time({YMD, {HH, MM, 0}});
%% Special case, current hour/minute given; it's rounded to the current second
convert_time(HH, MM, {_YMD, {HH, MM, _}} = Now, tomorrow) ->
	convert_time(Now);
%% Otherwise, it's yesterday...
convert_time(HH, MM, {YMD, _}, yesterday) ->
	convert_time({add_days(YMD, -1), {HH, MM, 0}});
%% ... or tomorrow.
convert_time(HH, MM, {YMD, _}, tomorrow) ->
	convert_time({add_days(YMD, 1), {HH, MM, 0}}).

%% Local time -> universal time | undefined
convert_time(LT) ->
	case calendar:local_time_to_universal_time_dst(LT) of
		[]       ->  undefined;
		[UT | _] -> {time, UT}
	end.

lowercase(S) -> [lc(C) || C <- S].
uppercase(S) -> [uc(C) || C <- S].

switchcase(S) -> [sc(C) || C <- S].

lc(C) when C >= $A, C =< $Z; C >= 1040, C =< 1071 -> C + 32;  % Latin and Cyrillic
lc(1025) -> 1105;											  % ё
lc(C) -> C.

uc(C) when C >= $a, C =< $z; C >= 1072, C =< 1103 -> C - 32;  % Latin and Cyrillic
uc(1105) -> 1025;											  % ё
uc(C) -> C.

sc(C) when C >= $a, C =< $z; C >= 1072, C =< 1103 -> C - 32;  % Latin and Cyrillic
sc(C) when C >= $A, C =< $Z; C >= 1040, C =< 1071 -> C + 32;
sc(1025) -> 1105;											  % ё
sc(1105) -> 1025;
sc(C) -> C.
