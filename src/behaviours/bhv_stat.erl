%%%-------------------------------------------------------------------
%%% File    : bhv_stat.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_stat).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).
-export([get_stat/0]).

-include("utf8.hrl").
-include("irc.hrl").

%%% === Subject to removal =================================================
-include_lib("stdlib/include/qlc.hrl").

-record(lstat, {ident  :: list(),
				lines  :: integer()}).
-record(cstat, {ident  :: list(),
				chars  :: integer()}).
-record(uinfo, {ident     :: list(),
				first_day :: {integer(), integer(), integer()}}).
%%% ========================================================================

init(_) -> undefined.

help(chancmd) ->
	[{"stat", "статистика пользователей"}];
help(privcmd) ->
	[{"stat", "пофапать на статистику тайно"}];
help(about) ->
	"Статистика пиздежа".

%%% === Subject to removal =================================================
%% We could use either `genevent' for counting (which includes public commands to bot) or
%% `msgevent' (which excludes "<botnick>: " part from appeals.
handle_event(genevent, {What, _, ?USER2(_, Ident), Msg}, _, _) when What =:= chanmsg; What =:= action ->
	update_stat(Ident, 1, length(Msg));
%%% ========================================================================
handle_event(cmdevent, {chancmd, Chan, ?USER2(_, Ident), ["stat" | _]}, _, _) ->
	ok = irc_conn:bulk_chanmsg(Chan, nohist, show_stat(Ident));
handle_event(cmdevent, {privcmd, ?USER2(Nick, Ident), ["stat" | _]}, _, _) ->
	ok = irc_conn:bulk_privmsg(Nick, nohist, show_stat(Ident));
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

%%% === Subject to removal =================================================
update_stat(Ident, Lines, Chars) ->
	mnesia:dirty_update_counter(lstat, Ident, Lines),
	mnesia:dirty_update_counter(cstat, Ident, Chars),
	mnesia:async_dirty(fun update_first_day/1, [Ident]),
	ok.

update_first_day(Ident) ->
	case mnesia:read(uinfo, Ident) of
		[] -> {Date, _} = erlang:localtime(),
			  mnesia:write(#uinfo{ident = Ident, first_day = Date});
		_  -> not_handled
	end.
%%% ========================================================================

-define(MIN_VISIBLE_STAT_LINES, 2000).  % Minimum number of lines user must have to be shown in stat.

show_stat(Ident) -> dump_stat(Ident, get_stat()).

get_stat() ->
	case erlbot_db:query_view({"stat", "by_user"}, [{group, true}]) of
		{_, _, _, Stat} -> filter_stat(Stat);
		{error, _E}     -> []
	end.

%% Filter out strangers and sort by total lines
filter_stat(Stat) ->
	lists:reverse(lists:keysort(2, [{I, S} || {_, I, [L | _] = S} <- Stat, 
											  L >= ?MIN_VISIBLE_STAT_LINES])).

dump_stat(Ident, StatLines) ->
	% idents are returned as binaries so compare with binary, too
	IdentBin = utf8:encode(Ident),
	Lines = [statline_to_list(IdentBin, Stat) || Stat <- StatLines],
	[" ************** Статистега ************************",
	 " Идент       | Сообщения | Символы | Длина | В день",
	 " --------------------------------------------------" | Lines] ++ 
		[" **************************************************"].

statline_to_list(Ident, {Ident, _} = S) -> [statline_to_list(S), " <==="];
statline_to_list(_, S)                  -> statline_to_list(S).

statline_to_list({Ident, [Lines, Chars, Days]}) ->
	io_lib:format(" ~-11s | ~9B | ~7B | ~-5.1f | ~6.1f", [format_ident(Ident), Lines, Chars, 
														  (Chars + 1)/(Lines + 1), Lines/erlang:max(Days, 1)]).

%% Strip leading ~ from ident
format_ident([$~ | Ident])        -> Ident;
format_ident(<<$~,Ident/binary>>) -> Ident;
format_ident(Ident)               -> Ident.
