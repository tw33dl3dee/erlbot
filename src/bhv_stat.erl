%%%-------------------------------------------------------------------
%%% File    : bhv_stat.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_stat).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).
-export([fix_stat/1, get_stat/0]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

-include_lib("stdlib/include/qlc.hrl").

-record(lstat, {ident  :: list(),
				lines  :: integer()}).
-record(cstat, {ident  :: list(),
				chars  :: integer()}).
-record(uinfo, {ident     :: list(),
				first_day :: {integer(), integer(), integer()}}).

init(_) -> 	
	ok = erlbot_db:init_table(lstat, [{disc_copies, [node()]}, {attributes, record_info(fields, lstat)}]),
	ok = erlbot_db:init_table(cstat, [{disc_copies, [node()]}, {attributes, record_info(fields, cstat)}]),
	ok = erlbot_db:init_table(uinfo, [{disc_copies, [node()]}, {attributes, record_info(fields, uinfo)}]),
	undefined.

help(chancmd) ->
	[{"stat", "статистика пользователей"}];
help(privcmd) ->
	[{"stat", "пофапать на статистику тайно"}];
help(about) ->
	"Статистика пиздежа".

%% We could use either `genevent' for counting (which includes public commands to bot) or
%% `msgevent' (which excludes "<botnick>: " part from appeals.
handle_event(genevent, {What, _, ?USER2(_, Ident), Msg}, _Irc) when What =:= chanmsg; What =:= action ->
	update_stat(Ident, 1, length(Msg));
%handle_event(msgevent, {_, _, ?USER2(_, Ident), Msg}, _Irc) ->
%	update_stat(Ident, 1, length(Msg));
handle_event(cmdevent, {chancmd, Chan, ?USER2(_, Ident), ["stat" | _]}, Irc) ->
	ok = irc_conn:bulk_chanmsg(Irc, Chan, nohist, show_stat(Ident));
handle_event(cmdevent, {privcmd, ?USER2(Nick, Ident), ["stat" | _]}, Irc) ->
	ok = irc_conn:bulk_privmsg(Irc, Nick, nohist, show_stat(Ident));
handle_event(_Type, _Event, _Irc) ->
	not_handled.

update_stat(Ident, Lines, Chars) ->
	mnesia:dirty_update_counter(lstat, Ident, Lines),
	mnesia:dirty_update_counter(cstat, Ident, Chars),
	mnesia:async_dirty(fun update_first_day/1, [Ident]),
	ok.

fix_stat(IrcName) ->
	irc_client:remove_behaviour(IrcName, ?MODULE),
	irc_client:add_behaviour(IrcName, ?MODULE),
	AllUsr = qlc:q([L#lstat.ident || L <- mnesia:table(lstat)]),
	mnesia:async_dirty(fun () -> [begin io:format("Fix ~s~n", [I]), 
										update_stat(I, 0, 0) 
								  end || I <- qlc:eval(AllUsr)] end).

update_first_day(Ident) ->
	case mnesia:read(uinfo, Ident) of
		[] -> {Date, _} = erlang:localtime(),
			  mnesia:write(#uinfo{ident = Ident, first_day = Date});
		_  -> not_handled
	end.

-define(MIN_VISIBLE_STAT_LINES, 100).  % Minimum number of lines user must have to be shown in stat.

show_stat(Ident) -> dump_stat(Ident, get_stat()).

get_stat() ->
	Q = qlc:q([{L#lstat.ident, L#lstat.lines, C#cstat.chars, U#uinfo.first_day} || 
				  L <- mnesia:table(lstat),
				  C <- mnesia:table(cstat),
				  U <- mnesia:table(uinfo),
				  L#lstat.ident =:= C#cstat.ident, L#lstat.ident =:= U#uinfo.ident,
				  L#lstat.lines > ?MIN_VISIBLE_STAT_LINES]),
	Qs = qlc:keysort(2, Q, [{order, descending}]),
	mnesia:async_dirty(fun () -> qlc:eval(Qs) end).

dump_stat(Ident, StatLines) ->
	Lines = [statline_to_list(Ident, Stat) || Stat <- StatLines],
	[" ************** Статистега ************************",
	 " Идент       | Сообщения | Символы | Длина | В день",
	 " --------------------------------------------------" | Lines] ++ 
		[" **************************************************"].

statline_to_list(Ident, {Ident, _, _, _} = S) -> [statline_to_list(S), " <==="];
statline_to_list(_, S)                        -> statline_to_list(S).

statline_to_list({Ident, Lines, Chars, FirstDay}) ->
	io_lib:format(" ~-11s | ~9B | ~7B | ~-5.1f | ~6.1f", [format_ident(Ident), Lines, Chars, 
														  (Chars + 1)/(Lines + 1), Lines/days_passed(FirstDay)]).

days_passed(FirstDay) ->
	{CurDate, _} = erlang:localtime(),
	case erlbot_util:date_diff(CurDate, FirstDay) of
		0 -> 1;
		D -> D
	end.

format_ident([$~ | Ident]) -> Ident;
format_ident(Ident)        -> Ident.
