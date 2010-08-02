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
	save_histent(Chan, [$~ | IrcState#irc_state.login], {chanmsg, IrcState#irc_state.nick, Msg});
handle_event(selfevent, {action, Chan, hist, Msg}, IrcState, _) ->
	save_histent(Chan, [$~ | IrcState#irc_state.login], {action, IrcState#irc_state.nick, Msg});
handle_event(chanevent, {topic, Chan, ?USER2(Nick, Ident), Topic}, _, _) ->
	save_histent(Chan, Ident, {topic, Nick, Topic});
handle_event(chanevent, {mytopic, Chan, _, Topic}, IrcState, _) ->
	save_histent(Chan, [$~ | IrcState#irc_state.login], {topic, IrcState#irc_state.nick, Topic});
handle_event(chanevent, {join, Chan, ?USER2(Nick, Ident)}, _, _) ->
	save_histent(Chan, Ident, {join, Nick});
handle_event(chanevent, {joined, Chan, Topic, _}, IrcState, _) ->
	save_histent(Chan, [$~ | IrcState#irc_state.login], {joined, IrcState#irc_state.nick, Topic});
handle_event(chanevent, {part, Chan, ?USER2(Nick, Ident), Reason}, _, _) ->
	save_histent(Chan, Ident, {part, Nick, Reason});
handle_event(chanevent, {parted, Chan}, IrcState, _) ->
	save_histent(Chan, [$~ | IrcState#irc_state.login], {part, IrcState#irc_state.nick, []});
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
		[{DocId, _, [Ident, _]} = LastByNick] ->	
			case trace_lastseen(Chan, {ident, Ident}) of
				[{DocId, _, _}] -> dump_lastseen([LastByNick], Chan);
				[LastByIdent]   -> dump_lastseen([LastByIdent, LastByNick], Chan)
			end
	end.

%% -> {DocId, [NickOrIdent, Chan, Ts], [Ident, Event]}
trace_lastseen(Chan, {ident, Ident}) ->
	ChanBin = utf8:encode(Chan),
	IdentBin = utf8:encode(Ident),
	case erlbot_db:query_view({"history", "by_user"}, [{startkey, [IdentBin, ChanBin, {[]}]}, 
													   {endkey, [IdentBin, ChanBin, 0]}, 
													   {descending, true}, {limit, 1}]) of
		{_, _, _, [Histent]} -> [Histent];
		_                    -> []
	end;
trace_lastseen(Chan, {nick, Nick}) ->
	ChanBin = utf8:encode(Chan),
	NickBin = utf8:encode(Nick),
	case erlbot_db:query_view({"history", "by_nick"}, [{startkey, [NickBin, ChanBin, {[]}]}, 
													   {endkey, [NickBin, ChanBin, 0]}, 
													   {descending, true}, {limit, 1}]) of
		{_, _, _, [Histent]} -> [Histent];
		_                    -> []
	end.

dump_lastseen(Histents, Chan) ->
	bhv_common:empty_check(Chan, Histents),
	dump_histents(long, [json_to_histent(H) || H <- lists:sort(Histents)], Chan).

%% WSTAT -- word statistics

%%% === Subject to removal =================================================
update_wstat(Chan, Ident, Msg) ->
	Words = erlbot_util:words(Msg, 2), % don't count 1-letter words
	mnesia:async_dirty(fun () ->
							   Cid = chanid(Chan),
							   Uid = userid(Ident),
							   [mnesia:dirty_update_counter(wstat, {Uid, Cid, W}, 1) || W <- Words],
							   ok
					   end).
%%% ========================================================================

show_wstat(Chan, Source, Target) ->
	Wstat = get_wstat(Chan, Target),
	dump_wstat(Wstat, Source).

get_wstat(Chan, {nick, TargetNick}) ->
	case trace_lastseen(Chan, {nick, TargetNick}) of
		[{_, _, [Ident, _]}] -> get_wstat(Chan, {ident, Ident});
		[]                   -> []
	end;
get_wstat(Chan, {ident, Ident}) ->
	case erlbot_db:query_view({"wstat", "by_user"}, 
							  [{key, [utf8:encode(Chan), utf8:encode(Ident)]}, 
							   {group, true}]) of
		{_, _, _, L} -> top_wstat(L);
		_ -> []
	end;
get_wstat(Chan, all) ->
	case erlbot_db:query_view({"wstat", "by_user"}, 
							  [{startkey, [utf8:encode(Chan), <<"">>]}, 
							   {endkey,   [utf8:encode(Chan), {[]}]},
							   {group, true}]) of
		{_, _, _, L} -> top_wstat(L);
		_ -> []
	end.

-define(WSTAT_MIN_COUNT, 10).         %% minimum occurence count for statistic inclusion
-define(WSTAT_MIN_TOTAL_COUNT, 100).  %% don't show users with total top count less than this

%% StatByUser is [{undefined, [Chan, Ident], [[Count, Word], ...]}]
top_wstat(StatByUser) ->
	[begin
		 TopWords = [{C, W} || [C, W] <- Words, C >= ?WSTAT_MIN_COUNT],
		 Total = lists:foldl(fun ({C, _}, S) -> C + S end, 0, TopWords),
		 {Ident, {Total, TopWords}}
	 end || {_, [_, Ident], Words} <- StatByUser].

dump_wstat(TopStat, Source) ->
	bhv_common:empty_check(Source, TopStat),
	[dump_wstat_user(Id, Total, Words, Source) || 
		{Id, {Total, Words}} <- TopStat, 
		Total >= ?WSTAT_MIN_TOTAL_COUNT],
	ok.

dump_wstat_user(Ident, _Total, Words, Chan) ->
	ok = irc_conn:chanmsg(Chan, hist,
						  [Ident, ": ", [[Word, " (", integer_to_list(Count), ") "] || {Count, Word} <- Words]]).

%%% === Subject to removal =================================================
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
%%% ========================================================================

%% HISTORY -- per-channel event history

%%% === Subject to removal =================================================
save_histent(Chan, Ident, Event) ->
	mnesia:async_dirty(fun () ->
							   mnesia:write(#histent{timestamp = timestamp(),
													 uid       = userid(Ident),
													 cid       = chanid(Chan),
													 event     = Event})
					   end),
	couchbeam_db:save_doc(erlbot_db, {histent_to_json({unix_timestamp(),
													   Ident, Chan, Event})}),
	ok.
%%% ========================================================================

show_history(Nick, Chan, Param) ->
	case parse_hist_param(Param) of
		undefined -> give_help(Nick);
		P -> print_hist_header(P, Nick, Chan),
			 Histents = get_history(Chan, P),
			 ok = dump_histents(short, Histents, Nick)
	end.

% Convert current datetime to timestamp suitable for storing in DB.
timestamp() -> 
	{YMD, HMS} = erlang:universaltime(),
	{_, _, Us} = erlang:now(),
	neg_timestamp({YMD, HMS, Us}).

unix_timestamp() ->
	{_, _, Us} = erlang:now(),
	{erlbot_util:unix_timestamp(erlang:universaltime()), Us}.

neg_timestamp({{Y, M, D}, {HH, MM, SS}, U}) -> 
	{{-Y, -M, -D}, {-HH, -MM, -SS}, -U}.

%%% === Subject to removal =================================================
userid(me) -> 0;
userid(Ident) ->
	Q = qlc:q([U#user.uid || U <- mnesia:table(user), U#user.ident =:= Ident]),
	case qlc:e(Q) of
		[Id | _] -> Id;
		[]   -> 
			Id = erlbot_db:sequence(user),
			mnesia:write(#user{uid = Id, ident = Ident}),
			Id
	end.

chanid(Channel) ->
	Q = qlc:q([Ch#chan.cid || Ch <- mnesia:table(chan), Ch#chan.name =:= Channel]),
	case qlc:e(Q) of
		[Id | _] -> Id;
		[]   -> 
			Id = erlbot_db:sequence(channel),
			mnesia:write(#chan{cid = Id, name = Channel}),
			Id
	end.
%%% ========================================================================

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

%% maximum number of history entries returned
%% (to prevent OOM for huge histories)
-define(MAX_HISTORY_LEN, 10000).

%% Trace by specified login count.
%% TODO: implement it!
get_history(_Chan, {login_count, _LoginCount}) -> [];
%% Trace by specified begin/end time.
get_history(Chan, {time, From, To}) ->
	ChanBin = utf8:encode(Chan),
	case erlbot_db:query_view({"history", "by_chan"}, 
							  [{startkey, [ChanBin, erlbot_util:unix_timestamp(From)]},
							   {endkey,   [ChanBin, erlbot_util:unix_timestamp(To)]},
							   {limit,    ?MAX_HISTORY_LEN}]) of
		{_, _, _, Histents} ->
			[json_to_histent(H) || H <- Histents];
		_ -> []
	end.

histent_to_json({{TsSec, TsUsec}, U, Ch, E}) ->
	Key = io_lib:format("event:~b.~6..0b", [TsSec, TsUsec]),
	[{<<"_id">>, utf8:encode(Key)},
	 {<<"timestamp">>, TsSec + TsUsec/1000000},
	 {<<"channel">>, utf8:encode(Ch)},
	 {<<"user">>, utf8:encode(U)},
	 {<<"event">>, histevent_to_json(E)}].

%% Each histent that comes from DB has format {DocId, [.... , Ts], [Ident, Event]}
json_to_histent({_DocId, Key, [_Ident, Event]}) ->
	Ts = lists:last(Key),
	{erlbot_util:from_unix_timestamp(Ts), json_to_histevent([atom | Event])}.

histevent_to_json(X) when is_tuple(X) -> [histevent_to_json(Y) || Y <- tuple_to_list(X)];
histevent_to_json(X) when is_atom(X)  -> list_to_binary(atom_to_list(X));
histevent_to_json(X) when is_list(X)  -> utf8:encode(X);
histevent_to_json(X) -> X.

json_to_histevent([atom, A | X])       -> json_to_histevent([binary_to_existing_atom(A, utf8) | X]);
json_to_histevent(X) when is_list(X)   -> list_to_tuple([json_to_histevent(Y) || Y <- X]);
json_to_histevent(X) when is_binary(X) -> utf8:decode(X);
json_to_histevent(X) -> X.

dump_histents(TimeFormat, Histents, Target) ->
	Lines = [histent_to_list(TimeFormat, H) || H <- Histents],
	ok = irc_conn:bulk_privmsg(Target, nohist, Lines).

%%% === Subject to removal =================================================
histent_to_list(TimeFormat, #histent{timestamp = TS, event = Event}) ->
	[timestamp_to_list(TimeFormat, TS), " ", event_to_list(Event)];
%%% ========================================================================
histent_to_list(TimeFormat, {Ts, Event}) ->
	[timestamp_to_list(TimeFormat, Ts), " ", event_to_list(Event)].

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

%% WCHAIN support (should be in bhv_wchain, actually, but uses #histent)

fix_wchain() ->
	Q = qlc:q([Msg ||
				  #histent{event = {Event, _, Msg}} <- mnesia:table(histent),
				  Event =:= chanmsg orelse Event =:= action]),
	mnesia:async_dirty(fun () -> [mnesia:delete({wchain, K}) || K <- mnesia:all_keys(wchain)],
								 [bhv_markov:update_wchain_text(Msg) || Msg <- qlc:eval(Q)] 
					   end).

%% CouchDB uploaders

unix_timestamp({YMD, HMS, U}) ->
	{erlbot_util:unix_timestamp({YMD, HMS}), U}.

couchdb_upload() ->
	mnesia:async_dirty(fun () -> couchdb_upload_all() end).

couchdb_upload_all() ->
	Q = qlc:q([{unix_timestamp(neg_timestamp(H#histent.timestamp)), 
				U#user.ident, Ch#chan.name, H#histent.event} ||
				  H  <- mnesia:table(histent),
				  Ch <- mnesia:table(chan),
				  U  <- mnesia:table(user),
				  Ch#chan.cid  =:= H#histent.cid,
				  U#user.uid =:= H#histent.uid]),
	[couchdb_save(E) || E <- lists:reverse(qlc:eval(Q))],
	ok.

couchdb_save(E) ->
	case catch histent_to_json(E) of
		{'EXIT', Reason} ->
			io:format("ERR ~p: ~p~n", [E, Reason]);
		Json ->
			couchbeam_db:save_doc(erlbot_db, {Json})
	end.
