%%%-------------------------------------------------------------------
%%% File    : bhv_history.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_history).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).
-export([fix_wstat/0, get_wstat/2, get_history/2, fix_wchain/0]).
-export([couchdb_upload/0]).

-include("utf8.hrl").
-include("irc.hrl").

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
	erlbot_db:create_sequence(),
	ok = erlbot_db:init_table(user, [{disc_copies, [node()]},
									 {index, [#user.ident]},
									 {attributes, record_info(fields, user)}]),
	ok = erlbot_db:init_table(chan, [{disc_copies, [node()]},
									 {index, [#chan.name]},
									 {attributes, record_info(fields, chan)}]),
	ok = erlbot_db:init_table(histent, [{disc_copies, [node()]},
										{type, ordered_set},
										{attributes, record_info(fields, histent)}]),
	ok = erlbot_db:init_table(wstat, [{disc_copies, [node()]},
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

handle_event(genevent, {chanmsg, Chan, ?USER2(Nick, Ident), Msg}, _, _) ->
	update_wstat(Chan, Ident, Msg),
	save_histent(Chan, Ident, {chanmsg, Nick, Msg});
handle_event(genevent, {action, Chan, ?USER2(Nick, Ident), Msg}, _, _) ->
	update_wstat(Chan, Ident, Msg),
	save_histent(Chan, Ident, {action, Nick, Msg});
handle_event(selfevent, {chanmsg, Chan, hist, Msg}, IrcState, _) ->
	save_histent(Chan, me, {chanmsg, IrcState#irc_state.nick, Msg});
handle_event(selfevent, {action, Chan, hist, Msg}, IrcState, _) ->
	save_histent(Chan, me, {action, IrcState#irc_state.nick, Msg});
handle_event(chanevent, {topic, Chan, ?USER2(Nick, Ident), Topic}, _, _) ->
	save_histent(Chan, Ident, {topic, Nick, Topic});
handle_event(chanevent, {mytopic, Chan, _, Topic}, IrcState, _) ->
	save_histent(Chan, me, {topic, IrcState#irc_state.nick, Topic});
handle_event(chanevent, {join, Chan, ?USER2(Nick, Ident)}, _, _) ->
	save_histent(Chan, Ident, {join, Nick});
handle_event(chanevent, {joined, Chan, Topic, _}, IrcState, _) ->
	save_histent(Chan, me, {joined, IrcState#irc_state.nick, Topic});
handle_event(chanevent, {part, Chan, ?USER2(Nick, Ident), Reason}, _, _) ->
	save_histent(Chan, Ident, {part, Nick, Reason});
handle_event(chanevent, {parted, Chan}, IrcState, _) ->
	save_histent(Chan, me, {part, IrcState#irc_state.nick, []});
handle_event(chanevent, {quit, Chan, ?USER2(Nick, Ident), Reason}, _, _) ->
	save_histent(Chan, Ident, {quit, Nick, Reason});
handle_event(chanevent, {kick, Chan, ?USER2(Nick1, Ident), Nick2, Reason}, _, _) ->
	save_histent(Chan, Ident, {kick, Nick1, Nick2, Reason});
handle_event(chanevent, {kicked, Chan, ?USER2(Nick, Ident), Reason}, IrcState, _) ->
	save_histent(Chan, Ident, {kick, Nick, IrcState#irc_state.nick, Reason});
handle_event(chanevent, {mode, Chan, ?USER2(Nick1, Ident), Mode, Nick2}, _, _) ->
	save_histent(Chan, Ident, {mode, Nick1, Mode, Nick2});
handle_event(chanevent, {mymode, Chan, ?USER2(Nick, Ident), Mode, _}, IrcState, _) ->
	save_histent(Chan, Ident, {mode, Nick, Mode, IrcState#irc_state.nick});
handle_event(chanevent, {nick, Chan, Nick2, ?USER2(Nick1, Ident)}, _, _) ->
	save_histent(Chan, Ident, {nick, Nick1, Nick2});
handle_event(cmdevent, {privcmd, ?USER(Nick), ["hist", Chan | Rest]}, _, _) when ?IS_CHAN(Chan) ->
	show_history(Nick, Chan, Rest);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["hist" | _]}, _, _) ->
	give_help(Nick);
handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["hist" | Rest]}, _, _) ->
	show_history(Nick, Chan, Rest);
handle_event(cmdevent, {chancmd, Chan, _, ["lastseen", TargetNick]}, _, _) ->
	show_lastseen(Chan, {nick, TargetNick});
handle_event(cmdevent, {chancmd, Chan, _, ["wstat"]}, _, _) ->
	show_wstat(Chan, Chan, all);
handle_event(cmdevent, {chancmd, Chan, _, ["wstat", TargetNick]}, _, _) ->
	show_wstat(Chan, Chan, {nick, TargetNick});
handle_event(cmdevent, {privcmd, ?USER(Nick), ["wstat", Chan]}, _, _) ->
	show_wstat(Chan, Nick, all);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["wstat", Chan, TargetNick]}, _, _) ->
	show_wstat(Chan, Nick, {nick, TargetNick});
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

give_help(Nick) ->
	irc_conn:bulk_privmsg(Nick, nohist, ["Ебани тебя оса..."]),
	ok = irc_conn:bulk_privmsg(Nick, nohist, verbose_help()).

%% LASTSEEN -- when user was last seen on channel

show_lastseen(Chan, Target) ->
	case trace_lastseen(Chan, Target) of
		[] -> dump_lastseen([], Chan);
		[#histent{uid = Uid} = LastByNick] ->	
			case trace_lastseen(Chan, {uid, Uid}) of
				[LastByNick] -> dump_lastseen([LastByNick], Chan);
				[LastById]   -> dump_lastseen([LastById, LastByNick], Chan)
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

dump_lastseen(Histents, Chan) ->
	bhv_common:empty_check(Chan, Histents),
	dump_histents(long, lists:sort(Histents), Chan).

%% WSTAT -- word statistics

update_wstat(Chan, Ident, Msg) ->
	Words = erlbot_util:words(Msg, 2), % don't count 1-letter words
	mnesia:async_dirty(fun () ->
							   Cid = chanid(Chan),
							   Uid = userid(Ident),
							   [mnesia:dirty_update_counter(wstat, {Uid, Cid, W}, 1) || W <- Words],
							   ok
					   end).

show_wstat(Chan, Source, Target) ->
	Wstat = get_wstat(Chan, Target),
	dump_wstat(Wstat, Source).

get_wstat(Chan, {nick, TargetNick}) ->
	case trace_lastseen(Chan, {nick, TargetNick}) of
		[#histent{uid = Uid}] -> get_wstat(Chan, {id, Uid});
		[]                    -> []
	end;
get_wstat(Chan, Target) ->
	mnesia:async_dirty(fun get_wstat_xact/2, [Chan, Target]).

-define(WSTAT_MIN_LEN, 5).     %% minimum word length for statistic inclusion
-define(WSTAT_MIN_COUNT, 15).  %% minimum occurence count for statistic inclusion

get_wstat_xact(Chan, {id, Uid}) ->
	[#user{ident = Ident}] = mnesia:read(user, Uid),
	[#chan{cid = Cid}] = mnesia:index_read(chan, Chan, #chan.name),
	Q = qlc:q([{Ident, Count, Word} || 
				  #wstat{key = {Uid_, Cid_, Word}, count = Count} <- mnesia:table(wstat),
				  length(Word) >= ?WSTAT_MIN_LEN,
				  Count >= ?WSTAT_MIN_COUNT,
				  Uid =:= Uid_, Cid =:= Cid_], [{cache, ets}]),
	Qs = qlc:keysort(2, Q),
	top_wstat(lists:reverse(qlc:eval(Qs)));
get_wstat_xact(Chan, all) ->
	[#chan{cid = Cid}] = mnesia:index_read(chan, Chan, #chan.name),
	Q = qlc:q([{U#user.ident, Count, Word} || 
				  #wstat{key = {Uid, Cid_, Word}, count = Count} <- mnesia:table(wstat),
				  U <- mnesia:table(user),
				  length(Word) >= ?WSTAT_MIN_LEN,
				  Count >= ?WSTAT_MIN_COUNT,
				  U#user.uid =:= Uid,
				  Cid_ =:= Cid], [{cache, ets}]),
	Qs = qlc:keysort(2, Q),
	top_wstat(lists:reverse(qlc:eval(Qs))).

-define(WSTAT_MAX, 10).              %% number of words to output in word statistic
-define(WSTAT_MIN_SHOW_COUNT, 100).  %% don't show users with total top count less than this

%% StatByUser is [{Ident, Count, Word}] sorted by Count desc.
top_wstat(StatByUser) ->
	Stat = lists:foldl(fun ({Id, C, W}, D) -> dict:append(Id, {C, W}, D) end, 
					   dict:new(), StatByUser),
	TopStat = dict:map(fun (_, V) -> Words = lists:sublist(V, ?WSTAT_MAX),
									 Total = lists:sum([C || {C, _} <- Words]),
									 {Total, Words}
					   end, Stat),
	lists:keysort(1, dict:to_list(TopStat)).

dump_wstat(TopStat, Source) ->
	bhv_common:empty_check(Source, TopStat),
	[dump_wstat_user(Id, Total, Words, Source) || 
		{Id, {Total, Words}} <- TopStat, 
		Total >= ?WSTAT_MIN_SHOW_COUNT],
	ok.

dump_wstat_user(Ident, _Total, Words, Chan) ->
	ok = irc_conn:chanmsg(Chan, hist,
						  [Ident, ": ", [[Word, " (", integer_to_list(Count), ") "] || {Count, Word} <- Words]]).

fix_wstat() ->
	Q = qlc:q([{Ch#chan.name, U#user.ident, Msg} ||
				  #histent{event = {Event, _, Msg}} = H <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  U <- mnesia:table(user),
				  Ch#chan.cid =:= H#histent.cid,
				  U#user.uid =:= H#histent.uid,
				  Event =:= chanmsg orelse Event =:= action]),
	mnesia:transaction(fun () -> [mnesia:delete({wstat, K}) || K <- mnesia:all_keys(wstat)],
								 [update_wstat(Ch, Id, M) || {Ch, Id, M} <- qlc:eval(Q)] 
					   end).

%% HISTORY -- per-channel event history

save_histent(Chan, Ident, Event) ->
	ok = mnesia:async_dirty(fun () ->
									mnesia:write(#histent{timestamp = timestamp(),
														  uid       = userid(Ident),
														  cid       = chanid(Chan),
														  event     = Event})
							end).

show_history(Nick, Chan, Param) ->
	case parse_hist_param(Param) of
		undefined -> give_help(Nick);
		P -> print_hist_header(P, Nick, Chan),
			 Histents = get_history(Chan, P),
			 ok = dump_histents(short, Histents, Nick)
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
			Id = erlbot_db:sequence(user),
			mnesia:write(#user{uid = Id, ident = Ident}),
			Id
	end.

chanid(Channel) ->
	Q = qlc:q([Ch#chan.cid || Ch <- mnesia:table(chan), Ch#chan.name =:= Channel]),
	case qlc:e(Q) of
		[Id] -> Id;
		[]   -> 
			Id = erlbot_db:sequence(channel),
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

parse_time([$- | HH], MM) -> erlbot_util:convert_time_rel(list_to_integer(HH), list_to_integer(MM));
parse_time(HH, MM)        -> erlbot_util:convert_time_abs(list_to_integer(HH), list_to_integer(MM), yesterday).

print_hist_header({login_count, LoginCount}, Nick, Chan) ->
	irc_conn:privmsg(Nick, nohist, erlbot_util:multiline("History for ~s by ~p login(s)", [Chan, LoginCount]));
print_hist_header({time, From, To}, Nick, Chan) ->
	irc_conn:privmsg(Nick, nohist, erlbot_util:multiline("History for ~s from ~p to ~p~n", [Chan, From, To])).

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

dump_histents(TimeFormat, Histents, Target) ->
	Lines = [histent_to_list(TimeFormat, H) || H <- Histents],
	ok = irc_conn:bulk_privmsg(Target, nohist, Lines).

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

fetch_first(Q) ->
	case mnesia:transaction(fun () -> C = qlc:cursor(Q),
									  A = qlc:next_answers(C, 1),
									  qlc:delete_cursor(C),
									  A
							end) of
		{atomic, Histent} -> Histent
	end.

%% WCHAIN support (should be in bhv_wchain, actually, but uses #histent)

fix_wchain() ->
	Q = qlc:q([Msg ||
				  #histent{event = {Event, _, Msg}} <- mnesia:table(histent),
				  Event =:= chanmsg orelse Event =:= action]),
	mnesia:async_dirty(fun () -> [mnesia:delete({wchain, K}) || K <- mnesia:all_keys(wchain)],
								 [bhv_markov:update_wchain_text(Msg) || Msg <- qlc:eval(Q)] 
					   end).

couchdb_upload() ->
	{C, Db} = erlbot_db:couchdb(),
	mnesia:async_dirty(fun () -> couchdb_upload_all(Db) end),
	couchbeam_db:close(C, Db).

unix_timestamp({YMD, HMS, U}) ->
	{erlbot_util:unix_timestamp({YMD, HMS}), U}.

couchdb_upload_all(Db) ->
	Q = qlc:q([{unix_timestamp(neg_timestamp(H#histent.timestamp)), 
				U#user.ident, Ch#chan.name, H#histent.event} ||
				  H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  U  <- mnesia:table(user),
				  Ch#chan.cid  =:= H#histent.cid,
				  U#user.uid =:= H#histent.uid]),
	[couchdb_save(Db, E) || E <- lists:reverse(qlc:eval(Q))],
	ok.

couchdb_save(Db, E) ->
	case catch histent_to_json(E) of
		{'EXIT', Reason} ->
			io:format("ERR ~p: ~p~n", [E, Reason]);
		Json ->
			couchbeam_db:save_doc(Db, {Json})
	end.

histent_to_json({{TsSec, TsUsec}, U, Ch, E}) ->
	Key = io_lib:format("event:~b.~6..0b", [TsSec, TsUsec]),
	[{<<"_id">>, utf8:encode(Key)},
	 {<<"timestamp">>, TsSec + TsUsec/1000000},
	 {<<"channel">>, utf8:encode(Ch)},
	 {<<"user">>, utf8:encode(U)},
	 {<<"event">>, histevent_to_json(E)}].

histevent_to_json(X) when is_tuple(X) -> [histevent_to_json(Y) || Y <- tuple_to_list(X)];
histevent_to_json(X) when is_atom(X)  -> list_to_binary(atom_to_list(X));
histevent_to_json(X) when is_list(X)  -> utf8:encode(X);
histevent_to_json(X) -> X.
