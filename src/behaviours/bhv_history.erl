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
-export([get_history/2]).
-export([resolve_nick/2]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) ->
	[{"hist <время>",				"история событий на канале (в приват)"},
	 {"lastseen <ник>",             "вывод последней активности данного пользователя"}];
help(privcmd) ->
	[{"hist <канал> <время>",		"история событий на канале (в приват)"}];
help(about) ->
	"История событий на канале".

verbose_help() ->
	["    время может задаваться как h.mm, h:mm, -h.mm, -h:mm",
	 "    (отрицательное время считается от текущего)"].

handle_event(genevent, {chanmsg, Chan, ?USER2(Nick, Ident), Msg}, _, _) ->
	save_histent(Chan, Ident, {chanmsg, Nick, Msg});
handle_event(genevent, {action, Chan, ?USER2(Nick, Ident), Msg}, _, _) ->
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
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

give_help(Nick) ->
	irc_conn:bulk_privmsg(Nick, nohist, ["Ебани тебя оса..."]),
	ok = irc_conn:bulk_privmsg(Nick, nohist, verbose_help()).

%%% Public API

%% Resolves nick to ident
resolve_nick(Chan, Nick) ->
	case trace_lastseen(Chan, {nick, Nick}) of
		[{_, _, [Ident, _]}] -> utf8:decode(Ident);
		[]                   -> undefined
	end.

%%% LASTSEEN -- when user was last seen on channel

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
	dump_histents(datetime, [json_to_histent(H) || H <- lists:sort(Histents)], Chan).

%%% HISTORY -- per-channel event history

save_histent(Chan, Ident, Event) ->
	Doc = histent_to_json(Chan, Ident, Event),
	couchbeam_db:save_doc(erlbot_db, Doc),
	ok.

histent_to_json(Chan, Ident, Event) ->
	{TsNum, TsStr} = erlbot_util:unix_timestamp(),
	{[{<<"_id">>, list_to_binary(["event:", TsStr])},
	  {timestamp, TsNum},
	  {channel,   utf8:encode(Chan)},
	  {user,      utf8:encode(Ident)},
	  {event,     histevent_to_json(Event)}]}.

histevent_to_json(X) when is_tuple(X) -> [histevent_to_json(Y) || Y <- tuple_to_list(X)];
histevent_to_json(X) when is_atom(X)  -> list_to_binary(atom_to_list(X));
histevent_to_json(X) when is_list(X)  -> utf8:encode(X);
histevent_to_json(X) -> X.

show_history(Nick, Chan, Param) ->
	case parse_hist_param(Param) of
		undefined -> give_help(Nick);
		P -> print_hist_header(P, Nick, Chan),
			 Histents = get_history(Chan, P),
			 ok = dump_histents(time, Histents, Nick)
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
	ChanBin = utf8:encode(Chan),
	Histents = erlbot_db:foldl_view(fun (H, L) -> [json_to_histent(H) | L] end, [],
									{"history", "by_chan"}, 
									[{startkey, [ChanBin, erlbot_util:unix_timestamp(From)]},
									 {endkey,   [ChanBin, erlbot_util:unix_timestamp(To)]}]),
	lists:reverse(Histents).

%% Each histent that comes from DB has format {DocId, [.... , Ts], [Ident, Event]}
json_to_histent({_DocId, Key, [_Ident, Event]}) ->
	Ts = lists:last(Key),
	{Ts, json_to_histevent([atom | Event])}.

json_to_histevent([atom, A | X])       -> json_to_histevent([binary_to_existing_atom(A, utf8) | X]);
json_to_histevent(X) when is_list(X)   -> list_to_tuple([json_to_histevent(Y) || Y <- X]);
json_to_histevent(X) when is_binary(X) -> utf8:decode(X);
json_to_histevent(X) -> X.

dump_histents(TimeFormat, Histents, Target) ->
	Lines = [histent_to_list(TimeFormat, H) || H <- Histents],
	ok = irc_conn:bulk_privmsg(Target, nohist, Lines).

histent_to_list(TimeFormat, {Ts, Event}) ->
	["[", erlbot_util:timestamp_to_list(local, TimeFormat, Ts), "] ", histevent_to_list(Event)].

histevent_to_list({chanmsg, Nick, Msg}) ->
	["<", Nick, "> ", Msg];
histevent_to_list({action, Nick, Msg}) ->
	["* ", Nick, " ", Msg];
histevent_to_list({topic, Nick, Topic}) ->
	["--- ", Nick, " высрал в топег: ", Topic];
histevent_to_list({join, Nick}) ->
	["=> пришел ", Nick];
histevent_to_list({joined, Nick, {[], _, _}}) ->
	["=> пришел ", Nick, " (топег пуст)"];
histevent_to_list({joined, Nick, {Topic, Author, _Ts}}) ->
	["=> пришел ", Nick, " (топег: `", Topic, "', установлен ", Author, ")"];
histevent_to_list({part, Nick, []}) ->
	["<= ушел ", Nick];
histevent_to_list({part, Nick, Reason}) ->
	["<= ушел ", Nick, " (", Reason, ")"];
histevent_to_list({quit, Nick, Reason}) ->
	["<== ушел нахуй ", Nick, " (", Reason, ")"];
histevent_to_list({kick, Nick1, Nick2, Reason}) ->
	["## ", Nick1, " пнул нахуй ", Nick2, " (", Reason, ")"];
histevent_to_list({mode, Nick1, Nick2, Mode}) ->
	["## ", Nick1, " дал ", Nick2, " права ", Mode];
histevent_to_list({nick, Nick1, Nick2}) ->
	["@@ ", Nick1, " ныне известен как ", Nick2];
histevent_to_list(Ev) ->
	["??? какая-то Неведомая Ебанная Хуйня: ", io_lib:format("~p", [Ev])].
