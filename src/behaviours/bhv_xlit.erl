%%%-------------------------------------------------------------------
%%% File    : bhv_xlit.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_xlit).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

%% Dict: Channel -> [{Nick, Message}] (last MAX_XLIT_LINES entries for each channel)
init(_) -> dict:new().

help(chancmd) ->
	[{"xlit", "транслитерация последних предложений, набранных в ошибочной раскладке (англ. -> рус.)"}];
help(privcmd) ->
	none;
help(about) ->
	"Транслитерация".

-define(MAX_XLIT_LINES, 5).

handle_event(msgevent, {_, Chan, ?USER(Nick), Msg}, _, Data) ->
	case dict:find(Chan, Data) of
		{ok, Hist} ->
			{ok, dict:store(Chan, lists:sublist([{Nick, Msg} | Hist], ?MAX_XLIT_LINES), Data)};
		error ->
			{ok, dict:store(Chan, [{Nick, Msg}], Data)}
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["xlit"]}, _, Data) ->
	case dict:find(Chan, Data) of
		{ok, Hist} ->
			xlit(Chan, lists:reverse(Hist)),
			{ok, dict:erase(Chan, Data)};
		error ->
			not_handled
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

xlit(Chan, Hist) ->
	{Nicks, Misspelled} = lists:unzip([{Nick, [L, $\n]} || {Nick, L} <- Hist, is_misspelled(L)]),
	Arg1 = integer_to_list(length(Misspelled)),
	% BUG: use code:priv_dir
	{success, Xlitted} = erlbot_util:execv("xlit2.pl", [Arg1], "priv/bin", Misspelled),
	ok = show_xlitted(Chan, Xlitted, Nicks).

%% BUG: this should never happen
show_xlitted(Chan, Xlitted, Nicks) when length(Xlitted) =/= length(Nicks) ->
	ok = bhv_common:error(Chan, [io_lib:format("~p =/= ~p", [length(Xlitted), length(Nicks)])]);
show_xlitted(Chan, Xlitted, Nicks) ->
	Out = [["#XLIT: <", Nick, "> ", L] || {L, Nick} <- lists:zip(Xlitted, Nicks), length(L) > 0],
	irc_conn:bulk_chanmsg(Chan, hist, Out).

-define(MIN_LATINS_RATIO, 0.5).  % Minimum ratio of latin letters that line must contain.

is_misspelled(S) ->
	case letter_count(S) of
		{0, 0}                                      -> false;
		{Lat, All} when Lat/All < ?MIN_LATINS_RATIO -> false;
		_                                           -> true
	end.

letter_count(S) ->
	lists:foldl(fun (C, {Latins, Letters}) when C >= $a, C =< $z -> {Latins + 1, Letters + 1};
					(C, {Latins, Letters}) when C >= $A, C =< $Z -> {Latins + 1, Letters + 1};
					(C, {Latins, Letters}) when C > 127          -> {Latins, Letters + 1};
					(_NonPrint, Count)                           -> Count
				end, {0, 0}, S).
