%%%-------------------------------------------------------------------
%%% File    : bhv_xlit.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_xlit).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> dict:new().

help(_) ->
	["!xlit : транслитерация последних предложений, набранных в ошибочной раскладке (англ. -> рус.)"].

-define(MAX_XLIT_LINES, 5).

handle_event(msgevent, {_, Chan, ?USER(Nick), Msg}, #irc{data = Data}) ->
	case dict:find(Chan, Data) of
		{ok, Hist} ->
			{ok, dict:store(Chan, lists:sublist([{Nick, Msg} | Hist], ?MAX_XLIT_LINES), Data)};
		error ->
			{ok, dict:store(Chan, [{Nick, Msg}], Data)}
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["xlit"]}, Irc) ->
	case dict:find(Chan, Irc#irc.data) of
		{ok, Hist} ->
			xlit(Irc, Chan, lists:reverse(Hist)),
			{ok, dict:erase(Chan, Irc#irc.data)};
		error ->
			not_handled
	end;
handle_event(_Type, _Event, _Irc) ->
	not_handled.

xlit(Irc, Chan, Hist) ->
	{Nicks, Misspelled} = lists:unzip([{Nick, [L, $\n]} || {Nick, L} <- Hist, is_misspelled(L)]),
	Arg1 = integer_to_list(length(Misspelled)),
	{success, Xlitted} = util:execv("xlit2.pl", [Arg1], ?SCRIPT_DIR, Misspelled),
	ok = show_xlitted(Irc, Chan, Xlitted, Nicks).

show_xlitted(Irc, Chan, Xlitted, Nicks) when length(Xlitted) =/= length(Nicks) ->
	ok = bhv_common:error(Irc, Chan, [io_lib:format("~p =/= ~p", [length(Xlitted), length(Nicks)])]);
show_xlitted(Irc, Chan, Xlitted, Nicks) ->
	Out = [["#XLIT: <", Nick, "> ", L] || {L, Nick} <- lists:zip(Xlitted, Nicks), length(L) > 0],
	irc_conn:async_chanmsg(Irc, Chan, Out).

-define(MIN_LATINS_RATIO, 0.5).  % Minimum ratio of latin letters that line must contain.

is_misspelled(S) ->
	case letter_count(S) of
		{0, 0}                                      -> false;
		{Lat, All} when Lat/All < ?MIN_LATINS_RATIO -> false;
		_                                           -> true
	end.

letter_count(S) ->
	lists:foldl(fun(C, {Latins, Letters}) ->
						case C of
							C when C >= $a, C =< $z; C >= $A, C =< $Z ->
								{Latins + 1, Letters + 1};
							C when C > 127 ->
								{Latins, Letters + 1};
							C ->
								{Latins, Letters}
						end
				end, {0, 0}, S).
