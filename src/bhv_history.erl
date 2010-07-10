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
-export([fix_wstat/1, get_wstat/2, get_history/2]).

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
-record(wstat, {key      :: {integer(), integer(), list()},  % {uid, cid, word}
				count    :: integer()}).

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
	ok = db_util:init_table(wstat, [{disc_copies, [node()]},
									{attributes, record_info(fields, wstat)}]),
	undefined.

help(chancmd) ->
	[{"hist <время>",				"история событий на канале (в приват)"},
	 {"wstat [<ник>]",              "статистика употребления слов"},
	 {"lastseen <ник>",             "вывод последней активности данного пользователя"}];
help(privcmd) ->
	[{"hist <канал> <время>",		"то же самое, но чтоб никто не узнал"}];
help(about) ->
	"История событий на канале".

verbose_help() ->
	["    время может задаваться как h.mm, h:mm, -h.mm, -h:mm",
	 "    (отрицательное время считается от текущего)"].

handle_event(genevent, {chanmsg, Chan, ?USER2(Nick, Ident), Msg}, _Irc) ->
	update_wstat(Chan, Ident, Msg),
	save_histent(Chan, Ident, {chanmsg, Nick, Msg});
handle_event(genevent, {action, Chan, ?USER2(Nick, Ident), Msg}, _Irc) ->
	update_wstat(Chan, Ident, Msg),
	save_histent(Chan, Ident, {action, Nick, Msg});
handle_event(selfevent, {chanmsg, Chan, hist, Msg}, Irc) ->
	save_histent(Chan, me, {chanmsg, my_nick(Irc), Msg});
handle_event(selfevent, {action, Chan, hist, Msg}, Irc) ->
	save_histent(Chan, me, {action, my_nick(Irc), Msg});
handle_event(chanevent, {topic, Chan, ?USER2(Nick, Ident), Topic}, _Irc) ->
	save_histent(Chan, Ident, {topic, Nick, Topic});
handle_event(chanevent, {mytopic, Chan, _, Topic}, Irc) ->
	save_histent(Chan, me, {topic, my_nick(Irc), Topic});
handle_event(chanevent, {join, Chan, ?USER2(Nick, Ident)}, _Irc) ->
	save_histent(Chan, Ident, {join, Nick});
handle_event(chanevent, {joined, Chan, Topic, _}, Irc) ->
	save_histent(Chan, me, {joined, my_nick(Irc), Topic});
handle_event(chanevent, {part, Chan, ?USER2(Nick, Ident), Reason}, _Irc) ->
	save_histent(Chan, Ident, {part, Nick, Reason});
handle_event(chanevent, {parted, Chan}, Irc) ->
	save_histent(Chan, me, {part, my_nick(Irc), []});
handle_event(chanevent, {quit, Chan, ?USER2(Nick, Ident), Reason}, _Irc) ->
	save_histent(Chan, Ident, {quit, Nick, Reason});
handle_event(chanevent, {kick, Chan, ?USER2(Nick1, Ident), Nick2, Reason}, _Irc) ->
	save_histent(Chan, Ident, {kick, Nick1, Nick2, Reason});
handle_event(chanevent, {kicked, Chan, ?USER2(Nick, Ident), Reason}, Irc) ->
	save_histent(Chan, Ident, {kick, Nick, my_nick(Irc), Reason});
handle_event(chanevent, {mode, Chan, ?USER2(Nick1, Ident), Mode, Nick2}, _Irc) ->
	save_histent(Chan, Ident, {mode, Nick1, Mode, Nick2});
handle_event(chanevent, {mymode, Chan, ?USER2(Nick, Ident), Mode, _}, Irc) ->
	save_histent(Chan, Ident, {mode, Nick, Mode, my_nick(Irc)});
handle_event(chanevent, {nick, Chan, Nick2, ?USER2(Nick1, Ident)}, _Irc) ->
	save_histent(Chan, Ident, {nick, Nick1, Nick2});
handle_event(cmdevent, {privcmd, ?USER(Nick), ["hist", Chan | Rest]}, Irc) when ?IS_CHAN(Chan) ->
	show_history(Nick, Chan, Rest, Irc);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["hist" | _]}, Irc) ->
	give_help(Nick, Irc);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["hist" | Rest]}, Irc) ->
	show_history(Nick, Chan, Rest, Irc);
handle_event(cmdevent, {chancmd, Chan, _, ["lastseen", TargetNick]}, Irc) ->
	show_lastseen(Chan, {nick, TargetNick}, Irc);
handle_event(cmdevent, {chancmd, Chan, _, ["wstat"]}, Irc) ->
	show_wstat(Chan, all, Irc);
handle_event(cmdevent, {chancmd, Chan, _, ["wstat", TargetNick]}, Irc) ->
	show_wstat(Chan, {nick, TargetNick}, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

give_help(Nick, Irc) ->
	irc_conn:bulk_privmsg(Irc, Nick, nohist, ["Ебани тебя оса..."]),
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, verbose_help()).

%% LASTSEEN -- when user was last seen on channel

show_lastseen(Chan, Target, Irc) ->
	case trace_lastseen(Chan, Target) of
		[] -> dump_lastseen([], Chan, Irc);
		[#histent{uid = Uid} = LastByNick] ->	
			case trace_lastseen(Chan, {uid, Uid}) of
				[LastByNick] -> dump_lastseen([LastByNick], Chan, Irc);
				[LastById]   -> dump_lastseen([LastById, LastByNick], Chan, Irc)
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

dump_lastseen(Histents, Chan, Irc) ->
	bhv_common:empty_check(Irc, Chan, Histents),
	dump_histents(long, lists:sort(Histents), Chan, Irc).

%% WSTAT -- word statistics

-define(WSTAT_MIN_LEN, 1).  %% minimum word length for statistic inclusion

update_wstat(Chan, Ident, Msg) ->
	Words = util:words(Msg, ?WSTAT_MIN_LEN),
	mnesia:async_dirty(fun () ->
							   Cid = chanid(Chan),
							   Uid = userid(Ident),
							   [mnesia:dirty_update_counter(wstat, {Uid, Cid, W}, 1) || W <- Words],
							   ok
					   end).

show_wstat(Chan, Target, Irc) ->
	Wstat = get_wstat(Chan, Target),
	dump_wstat(Chan, Wstat, Irc).

get_wstat(Chan, {nick, TargetNick}) ->
	case trace_lastseen(Chan, {nick, TargetNick}) of
		[#histent{uid = Uid}] -> get_wstat(Chan, {id, Uid});
		[]                    -> []
	end;
get_wstat(Chan, {id, Uid}) ->
	[#user{ident = Ident}] = mnesia:dirty_read(user, Uid),
	Q = qlc:q([{Ident, Count, Word} || 
				  #wstat{key = {_, Cid, Word}, count = Count}  <- mnesia:table(wstat),
				  Ch <- mnesia:table(chan),
				  U <- mnesia:table(user),
				  Ch#chan.name =:= Chan,
				  Ch#chan.cid =:= Cid,
				  U#user.uid =:= Uid]),
	Qs = qlc:keysort(2, Q),
	mnesia:async_dirty(fun () -> lists:reverse(qlc:eval(Qs)) end);
get_wstat(Chan, all) ->
	Q = qlc:q([{U#user.ident, Count, Word} || 
				  #wstat{key = {Uid, Cid, Word}, count = Count}  <- mnesia:table(wstat),
				  Ch <- mnesia:table(chan),
				  U <- mnesia:table(user),
				  Ch#chan.name =:= Chan,
				  Ch#chan.cid =:= Cid,
				  U#user.uid =:= Uid]),
	Qs = qlc:keysort(2, Q),
	mnesia:async_dirty(fun () -> lists:reverse(qlc:eval(Qs)) end).

-define(WSTAT_MAX, 5).  % number of words to output in word statistic

%% StatByUser is [{Ident, Count, Word}] sorted by Count desc.
dump_wstat(StatByUser, Chan, Irc) ->
	bhv_common:empty_check(Irc, Chan, StatByUser),
	Stat = lists:foldl(fun ({Id, C, W}, D) -> dict:append(Id, {C, W}, D) end, 
					   dict:new(), StatByUser),
	TopStat = dict:map(fun (_, V) -> lists:sublist(V, ?WSTAT_MAX) end, Stat),
	[dump_wstat_user(Id, Words, Chan, Irc) || {Id, Words} <- lists:keysort(1, dict:to_list(TopStat))],
	ok.

dump_wstat_user(Ident, Words, Chan, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, hist,
						  [Ident, ": ", [[Word, " (", integer_to_list(Count), ") "] || {Count, Word} <- Words]]).

fix_wstat(IrcName) ->
	irc_client:remove_behaviour(IrcName, ?MODULE),
	irc_client:add_behaviour(IrcName, ?MODULE),
	Q = qlc:q([{Ch#chan.name, U#user.ident, Msg} ||
				  #histent{event = {Event, _, Msg}} = H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  U <- mnesia:table(user),
				  Ch#chan.cid =:= H#histent.cid,
				  U#user.uid =:= H#histent.uid,
				  Event =:= chanmsg orelse Event =:= action]),
	mnesia:transaction(fun () -> [update_wstat(Ch, Id, M) || {Ch, Id, M} <- qlc:eval(Q)] end).

%% HISTORY -- per-channel event history

save_histent(Chan, Ident, Event) ->
	ok = mnesia:async_dirty(fun () ->
									mnesia:write(#histent{timestamp = timestamp(),
														  uid       = userid(Ident),
														  cid       = chanid(Chan),
														  event     = Event})
							end).

show_history(Nick, Chan, Param, Irc) ->
	case parse_hist_param(Param) of
		undefined -> give_help(Nick, Irc);
		P -> print_hist_header(P, Nick, Chan, Irc),
			 Histents = get_history(Chan, P),
			 ok = dump_histents(short, Histents, Nick, Irc)
	end.

% Convert current datetime to timestamp suitable for storing in DB.
timestamp() -> timestamp(erlang:universaltime()).

% Convert datetime to timestamp suitable for storing in DB.
timestamp({YMD, HMS}) ->
	{_, _, U} = erlang:now(),
	neg_timestamp({YMD, HMS, U}).

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

parse_hist_param([])           -> {login_count, 1};
parse_hist_param([Arg1])       -> fix_hist_param(parse_time_or_number(Arg1));
parse_hist_param([Arg1, Arg2]) -> fix_hist_param([parse_time_or_number(Arg1), parse_time_or_number(Arg2)]);
parse_hist_param(_)            -> undefined.

fix_hist_param({number, X})                -> {login_count, X};
fix_hist_param({time, From})               -> {time, From, erlang:universaltime()};
fix_hist_param([{time, From}, {time, To}]) -> {time, From, To};
fix_hist_param(_)                          -> undefined.

parse_time_or_number(S) ->
	case re:run(S, "^(-?\\d?\\d)[:\\.](\\d\\d)", [unicode, {capture, all_but_first, list}]) of 
		{match, [HH, MM]} -> parse_time(HH, MM);
		nomatch           -> parse_number(S)
	end.

parse_number(S) ->
	case catch list_to_integer(S) of
		X when is_integer(X), X > 0 -> {number, X};
		_                           -> undefined
	end.

parse_time([$- | HH], MM) -> util:convert_time_rel(list_to_integer(HH), list_to_integer(MM));
parse_time(HH, MM)        -> util:convert_time_abs(list_to_integer(HH), list_to_integer(MM), yesterday).

print_hist_header({login_count, LoginCount}, Nick, Chan, Irc) ->
	irc_conn:privmsg(Irc, Nick, nohist, util:multiline("History for ~s by ~p login(s)", [Chan, LoginCount]));
print_hist_header({time, From, To}, Nick, Chan, Irc) ->
	irc_conn:privmsg(Irc, Nick, nohist, util:multiline("History for ~s from ~p to ~p~n", [Chan, From, To])).

%% Trace by specified login count.
%% TODO: implement it!
get_history(_Chan, {login_count, _LoginCount}) -> [];
%% Trace by specified begin/end time.
get_history(Chan, {time, From, To}) ->
	Q = qlc:q([H#histent{timestamp = neg_timestamp(H#histent.timestamp)} ||
				  H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  Ch#chan.cid  =:= H#histent.cid,
				  Ch#chan.name =:= Chan,
				  H#histent.timestamp < timestamp(From),
				  H#histent.timestamp > timestamp(To)], [{join, lookup}]),
	case mnesia:transaction(fun () -> lists:reverse(qlc:eval(Q)) end) of
		{atomic, Histents} -> Histents
	end.

dump_histents(TimeFormat, Histents, Target, Irc) ->
	Lines = [histent_to_list(TimeFormat, H) || H <- Histents],
	ok = irc_conn:bulk_privmsg(Irc, Target, nohist, Lines).

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

%% Utility

my_nick(#irc{nick = Nick}) -> Nick.

fetch_first(Q) ->
	case mnesia:transaction(fun () -> C = qlc:cursor(Q),
									  A = qlc:next_answers(C, 1),
									  qlc:delete_cursor(C),
									  A
							end) of
		{atomic, Histent} -> Histent
	end.
