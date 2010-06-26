%%%-------------------------------------------------------------------
%%% File    : bhv_history.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_history).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

-include_lib("stdlib/include/qlc.hrl").

-record(user, {uid    :: integer(),
			   ident  :: list()}). 
-record(chan, {cid   :: integer(),
			   name  :: list()}). 
-record(histent, {timestamp  :: {{integer(), integer(), integer()}, 
								 {integer(), integer(), integer()}, 
								 integer()},
				  uid        :: integer(),
				  cid        :: integer(),
				  event      :: tuple()}).

init(_) -> 
	db_util:create_sequence(),
	ok = db_util:init_table(user, [{disc_copies, [node()]},
								   {index, [#user.ident]},
								   {attributes, record_info(fields, user)}]),
	ok = db_util:init_table(chan, [{disc_copies, [node()]},
								   {index, [#chan.name]},
								   {attributes, record_info(fields, chan)}]),
	ok = db_util:init_table(histent, [{disc_copies, [node()]},
									  {type, ordered_set},
									  {attributes, record_info(fields, histent)}]),
	undefined.

help(chancmd) ->
	[{"hist <время>",				"история событий на канале (в приват)"},
	 {"lastseen <ник>",             "вывод последней активности данного пользователя"}];
help(privcmd) ->
	[{"hist <канал> <время>",		"то же самое, но чтоб никто не узнал"}];
help(about) ->
	"История событий на канале".

verbose_help() ->
	["    время может задаваться как h.mm, h:mm, -h.mm, -h:mm",
	 "    (отрицательное время считается от текущего)"].

handle_event(genevent, {chanmsg, Chan, ?USER2(Nick, Ident), Msg}, _Irc) ->
	save_histent(Chan, Ident, {chanmsg, Nick, Msg});
handle_event(genevent, {action, Chan, ?USER2(Nick, Ident), Msg}, _Irc) ->
	save_histent(Chan, Ident, {action, Nick, Msg});
handle_event(selfevent, {chanmsg, Chan, hist, Msg}, Irc) ->
	save_histent(Chan, me, {chanmsg, me(Irc), Msg});
handle_event(selfevent, {action, Chan, hist, Msg}, Irc) ->
	save_histent(Chan, me, {action, me(Irc), Msg});
handle_event(chanevent, {topic, Chan, ?USER2(Nick, Ident), Topic}, _Irc) ->
	save_histent(Chan, Ident, {topic, Nick, Topic});
handle_event(chanevent, {mytopic, Chan, _, Topic}, Irc) ->
	save_histent(Chan, me, {topic, me(Irc), Topic});
handle_event(chanevent, {join, Chan, ?USER2(Nick, Ident)}, _Irc) ->
	save_histent(Chan, Ident, {join, Nick});
handle_event(chanevent, {joined, Chan, Topic, _}, Irc) ->
	save_histent(Chan, me, {joined, me(Irc), Topic});
handle_event(chanevent, {part, Chan, ?USER2(Nick, Ident), Reason}, _Irc) ->
	save_histent(Chan, Ident, {part, Nick, Reason});
handle_event(chanevent, {parted, Chan}, Irc) ->
	save_histent(Chan, me, {part, me(Irc), []});
handle_event(chanevent, {quit, Chan, ?USER2(Nick, Ident), Reason}, _Irc) ->
	save_histent(Chan, Ident, {quit, Nick, Reason});
handle_event(chanevent, {kick, Chan, ?USER2(Nick1, Ident), Nick2, Reason}, _Irc) ->
	save_histent(Chan, Ident, {kick, Nick1, Nick2, Reason});
handle_event(chanevent, {kicked, Chan, ?USER2(Nick, Ident), Reason}, Irc) ->
	save_histent(Chan, Ident, {kick, Nick, me(Irc), Reason});
handle_event(chanevent, {mode, Chan, ?USER2(Nick1, Ident), Mode, Nick2}, _Irc) ->
	save_histent(Chan, Ident, {mode, Nick1, Mode, Nick2});
handle_event(chanevent, {mymode, Chan, ?USER2(Nick, Ident), Mode, _}, Irc) ->
	save_histent(Chan, Ident, {mode, Nick, Mode, me(Irc)});
handle_event(chanevent, {nick, Chan, Nick2, ?USER2(Nick1, Ident)}, _Irc) ->
	save_histent(Chan, Ident, {nick, Nick1, Nick2});
handle_event(cmdevent, {privcmd, ?USER(Nick), ["hist", Chan | Rest]}, Irc) when ?IS_CHAN(Chan) ->
	show_history(Nick, Chan, Rest, Irc);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["hist" | _]}, Irc) ->
	give_help(Nick, Irc);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["hist" | Rest]}, Irc) ->
	show_history(Nick, Chan, Rest, Irc);
handle_event(cmdevent, {chancmd, Chan, _, ["lastseen", TargetNick]}, Irc) ->
	show_lastseen(Chan, TargetNick, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

give_help(Nick, Irc) ->
	irc_conn:bulk_privmsg(Irc, Nick, nohist, ["Ебани тебя оса..."]),
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, verbose_help()).

save_histent(Chan, Ident, Event) ->
	ok = mnesia:async_dirty(fun () ->
									mnesia:write(#histent{timestamp = timestamp(),
														  uid       = userid(Ident),
														  cid       = chanid(Chan),
														  event     = Event})
							end).

me(#irc{nick = Nick}) -> Nick.

% Convert datetime to timestamp suitable for storing in DB.
timestamp({YMD, HMS}) ->
	{_, _, U} = erlang:now(),
	neg_timestamp({YMD, HMS, U}).

% Convert current datetime to timestamp suitable for storing in DB.
timestamp() ->
	timestamp(erlang:universaltime()).

neg_timestamp({{Y, M, D}, {HH, MM, SS}, U}) ->
	{{-Y, -M, -D}, {-HH, -MM, -SS}, -U}.

userid(me) -> 0;
userid(Ident) ->
	Q = qlc:q([U#user.uid || U <- mnesia:table(user), U#user.ident =:= Ident]),
	case qlc:e(Q) of
		[Id] -> Id;
		[]   -> 
			Id = db_util:sequence(user),
			mnesia:write(#user{uid = Id, ident = Ident}),
			Id
	end.

chanid(Channel) ->
	Q = qlc:q([Ch#chan.cid || Ch <- mnesia:table(chan), Ch#chan.name =:= Channel]),
	case qlc:e(Q) of
		[Id] -> Id;
		[]   -> 
			Id = db_util:sequence(channel),
			mnesia:write(#chan{cid = Id, name = Channel}),
			Id
	end.

parse_time_or_number(S) ->
	case re:run(S, "^(-?\\d?\\d)[:\\.](\\d\\d)", [unicode, {capture, all_but_first, list}]) of 
		{match, [HH, MM]} ->
			parse_time(HH, MM);
		nomatch ->
			parse_number(S)
	end.

parse_number(S) ->
	case catch list_to_integer(S) of
		X when is_integer(X), X > 0 ->
			{number, X};
		_ ->
			undefined
	end.

parse_time([$- | HH], MM) ->
	util:convert_time_rel(list_to_integer(HH), list_to_integer(MM));
parse_time(HH, MM) ->
	util:convert_time_abs(list_to_integer(HH), list_to_integer(MM), yesterday).

show_history(Nick, Chan, [], Irc) ->
	history(Nick, Chan, {number, 1}, Irc);
show_history(Nick, Chan, [Arg1], Irc) ->
	history(Nick, Chan, parse_time_or_number(Arg1), Irc);
show_history(Nick, Chan, [Arg1, Arg2], Irc) ->
	history(Nick, Chan, parse_time_or_number(Arg1), parse_time_or_number(Arg2), Irc);
show_history(Nick, Chan, _, Irc) ->
	history(Nick, Chan, undefined, Irc).

history(Nick, Chan, {number, X}, Irc) ->
	trace_history(list, Nick, Chan, X, Irc);
history(Nick, Chan, {time, From}, Irc) ->
	trace_history(list, Nick, Chan, From, erlang:universaltime(), Irc);
history(Nick, _, undefined, Irc) ->
	give_help(Nick, Irc).

history(Nick, Chan, {time, From}, {time, To}, Irc) ->
	trace_history(list, Nick, Chan, From, To, Irc);
history(Nick, _, _, _, Irc) ->
	give_help(Nick, Irc).

%% Trace by specified login count.
trace_history(_Method, Nick, Chan, LoginCount, Irc) ->
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, util:multiline("History for ~s by ~p login(s)", [Chan, LoginCount])).

%% Trace by specified begin/end time.
trace_history(Method, Nick, Chan, From, To, Irc) ->
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, util:multiline("History for ~s from ~p to ~p~n", [Chan, From, To])),
	Q = qlc:q([H#histent{timestamp = neg_timestamp(H#histent.timestamp)} ||
				  H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  Ch#chan.cid  =:= H#histent.cid,
				  Ch#chan.name =:= Chan,
				  H#histent.timestamp < timestamp(From),
				  H#histent.timestamp > timestamp(To)], [{join, lookup}]),
	io:format("QUERY ~s~n", [qlc:info(Q)]),
	{T, {atomic, ok}} = timer:tc(mnesia, transaction, [fun () -> fetch_history(Method, Q, Nick, Irc) end]),
	ok = io:format("QUERY TOOK ~p usec~n", [T]).

fetch_history(list, Q, Nick, Irc) ->
	R = lists:reverse(qlc:eval(Q)),
	Lines = [histent_to_list(short, H) || H <- R],
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, Lines);
fetch_history(cursor, Q, Nick, Irc) ->
	Qs = qlc:keysort(1, Q),
	C = qlc:cursor(Qs),
	ok = dump_history(C, Nick, Irc).

fetch_first(Q) ->
	{atomic, Histent} = mnesia:transaction(fun () -> C = qlc:cursor(Q),
													 A = qlc:next_answers(C, 1),
													 io:format("==FETCH ~p==~n", [A]),
													 qlc:delete_cursor(C),
													 A
										   end),
	Histent.

show_lastseen(Chan, TargetNick, Irc) ->
	case trace_lastseen(Chan, {nick, TargetNick}) of
		[] -> dump_lastseen({}, Chan, Irc);
		[#histent{uid = Uid} = LastByNick] ->	
			case trace_lastseen(Chan, {uid, Uid}) of
				[LastByNick] -> dump_lastseen({LastByNick}, Chan, Irc);
				[LastById]   -> dump_lastseen({LastById, LastByNick}, Chan, Irc)
			end
	end.

-define(HIST_EVENT_BYNICK(Event, Nick),
		element(2, Event) =:= Nick orelse
		(element(3, Event) =:= Nick andalso 
		 (element(1, Event) =:= kick orelse element(1, Event) =:= nick))).

trace_lastseen(Chan, {uid, Uid}) ->
	Q = qlc:q([H#histent{timestamp = neg_timestamp(H#histent.timestamp)} ||
				  H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  Ch#chan.cid  =:= H#histent.cid,
				  Ch#chan.name =:= Chan,
				  H#histent.uid =:= Uid], [{join, lookup}]),
	fetch_first(Q);
trace_lastseen(Chan, {nick, Nick}) ->
	Q  = qlc:q([H#histent{timestamp = neg_timestamp(H#histent.timestamp)} ||
				  H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  Ch#chan.cid  =:= H#histent.cid,
				  Ch#chan.name =:= Chan,
				  ?HIST_EVENT_BYNICK(H#histent.event, Nick)], [{join, lookup}]),
	fetch_first(Q).

dump_lastseen({}, Chan, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, hist, bhv_common:empty_check([]));
dump_lastseen({SingleEntry}, Chan, Irc) ->
	ok = dump_histents(long, [SingleEntry], Chan, Irc);
dump_lastseen({Entry1, Entry2}, Chan, Irc) when Entry1 < Entry2 ->
	ok = dump_histents(long, [Entry1, Entry2], Chan, Irc);
dump_lastseen({Entry1, Entry2}, Chan, Irc) ->
	ok = dump_histents(long, [Entry2, Entry1], Chan, Irc).

-define(HIST_CHUNKLEN, 100).

dump_history(Cursor, Nick, Irc)	->
	dump_history(Cursor, qlc:next_answers(Cursor, ?HIST_CHUNKLEN), Nick, Irc).

dump_history(Cursor, [], _, _) ->
	ok = qlc:delete_cursor(Cursor);
dump_history(Cursor, Results, Nick, Irc) ->
	dump_histents(short, Results, Nick, Irc),
	dump_history(Cursor, qlc:next_answers(Cursor, ?HIST_CHUNKLEN), Nick, Irc).

dump_histents(TimeFormat, Histents, Target, Irc) ->
	Lines = [histent_to_list(TimeFormat, H) || H <- Histents],
	irc_conn:bulk_privmsg(Irc, Target, nohist, Lines).

histent_to_list(TimeFormat, #histent{timestamp = TS, event = Event}) ->
	[timestamp_to_list(TimeFormat, TS), " ", event_to_list(Event)].

timestamp_to_list(long, {YMD, HMS, _}) ->
	{{Y, M, D}, {HH, MM, SS}} = calendar:universal_time_to_local_time({YMD, HMS}),
	io_lib:format("[~2..0b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b]", [Y rem 1000, M, D, HH, MM, SS]);
timestamp_to_list(short, {YMD, HMS, _}) ->
	{_, {HH, MM, SS}} = calendar:universal_time_to_local_time({YMD, HMS}),
	io_lib:format("[~2..0b:~2..0b:~2..0b]", [HH, MM, SS]).

event_to_list({chanmsg, Nick, Msg}) ->
	["<", Nick, "> ", Msg];
event_to_list({action, Nick, Msg}) ->
	["* ", Nick, " ", Msg];
event_to_list({topic, Nick, Topic}) ->
	["--- ", Nick, " высрал в топег: ", Topic];
event_to_list({join, Nick}) ->
	["=> пришел ", Nick];
event_to_list({joined, Nick, {[], _, _}}) ->
	["=> пришел ", Nick, " (топег пуст)"];
event_to_list({joined, Nick, {Topic, Author, _Ts}}) ->
	["=> пришел ", Nick, " (топег: `", Topic, "', установлен ", Author, ")"];
event_to_list({part, Nick, []}) ->
	["<= ушел ", Nick];
event_to_list({part, Nick, Reason}) ->
	["<= ушел ", Nick, " (", Reason, ")"];
event_to_list({quit, Nick, Reason}) ->
	["<== ушел нахуй ", Nick, " (", Reason, ")"];
event_to_list({kick, Nick1, Nick2, Reason}) ->
	["## ", Nick1, " пнул нахуй ", Nick2, " (", Reason, ")"];
event_to_list({mode, Nick1, Nick2, Mode}) ->
	["## ", Nick1, " дал ", Nick2, " права ", Mode];
event_to_list({nick, Nick1, Nick2}) ->
	["@@ ", Nick1, " ныне известен как ", Nick2];
event_to_list(Ev) ->
	["??? какая-то Неведомая Ебанная Хуйня: ", io_lib:format("~p", [Ev])].
