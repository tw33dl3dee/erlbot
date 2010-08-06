%%%-------------------------------------------------------------------
%%% File    : bhv_wstat.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  6 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_wstat).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).
-export([get_wstat/2]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) ->
	[{"wstat [<ник>]",              "статистика употребления слов"}];
help(privcmd) ->
	none;
help(about) ->
	"Статистика пиздежа, по словам".

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

show_wstat(Chan, Source, Target) ->
	Wstat = get_wstat(Chan, Target),
	dump_wstat(Wstat, Source).

get_wstat(Chan, {nick, TargetNick}) ->
	case bhv_history:trace_lastseen(Chan, {nick, TargetNick}) of
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
