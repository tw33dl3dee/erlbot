%%%-------------------------------------------------------------------
%%% File    : bhv_stat.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_stat).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(lstat, {ident  :: list(),
				lines  :: integer()}).
-record(cstat, {ident  :: list(),
				chars  :: integer()}).

init(_) -> 	
	{atomic, ok} = db_util:init_table(lstat, [{disc_copies, [node()]},
											  {attributes, record_info(fields, lstat)}]),
	{atomic, ok} = db_util:init_table(cstat, [{disc_copies, [node()]},
											  {attributes, record_info(fields, cstat)}]),
	undefined.

%% We could use either `genevent' for counting (which includes public commands to bot) or
%% `msgevent' (which excludes "<botnick>: " part from appeals.
handle_event(genevent, {What, _, ?USER2(_, Ident), Msg}, _Irc) when What =:= chanmsg; What =:= action ->
	update_stat(Ident, 1, length(Msg));
%handle_event(msgevent, {_, _, ?USER2(_, Ident), Msg}, _Irc) ->
%	update_stat(Ident, 1, length(Msg));
handle_event(cmdevent, {chancmd, Chan, ?USER2(_, Ident), ["stat" | _]}, Irc) ->
	ok = irc_conn:async_chanmsg(Irc, Chan, get_stat(Ident));
handle_event(cmdevent, {privcmd, ?USER2(Nick, Ident), ["stat" | _]}, Irc) ->
	ok = irc_conn:async_privmsg(Irc, Nick, get_stat(Ident));
handle_event(_Type, _Event, _Irc) ->
	not_handled.

update_stat(Ident, Lines, Chars) ->
	mnesia:dirty_update_counter(lstat, Ident, Lines),
	mnesia:dirty_update_counter(cstat, Ident, Chars),
	ok.

-define(MIN_VISIBLE_STAT_LINES, 100).  % Minimum number of lines user must have to be shown in stat.

get_stat(Ident) ->
	Q = qlc:q([{L#lstat.ident, L#lstat.lines, C#cstat.chars} || 
				  L <- mnesia:table(lstat),
				  C <- mnesia:table(cstat),
				  L#lstat.ident =:= C#cstat.ident,
				  L#lstat.lines > ?MIN_VISIBLE_STAT_LINES]),
	Qs = qlc:keysort(2, Q, [{order, descending}]),
	mnesia:async_dirty(fun () -> dump_stat(Ident, qlc:eval(Qs)) end).

dump_stat(Ident, StatLines) ->
	Lines = [statline_to_list(Ident, Stat) || Stat <- StatLines],
	[" ************* Статистега **************",
	 " Пользователь    | Сообщения |   Символы",
	 " ---------------------------------------" | Lines] ++ 
		[" ***************************************"].

statline_to_list(Ident, {Ident, _, _} = S) ->
	[statline_to_list(S), " <==="];
statline_to_list(_, S) ->
	statline_to_list(S).

statline_to_list({Ident, Lines, Chars}) ->
	io_lib:format(" ~-15s | ~9B | ~9B", [format_ident(Ident), Lines, Chars]).

format_ident([$~ | Ident]) -> Ident;
format_ident(Ident)        -> Ident.
