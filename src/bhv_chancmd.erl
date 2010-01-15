%%%-------------------------------------------------------------------
%%% File    : bhv_chancmd.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_chancmd).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

%% Data is dict mapping Channel -> {RemainingLines, StartTime}
init(_) -> dict:new().

-define(MAX_LINES, 2).        %% How many lines after `selfevent' are considered possible appeal.
-define(APPEAL_TIMEOUT, 30).  %% How much time (in sec) can pass after bot stops accepting smart appeal

% User command ends smart appeal
handle_event(genevent, {chanmsg, Chan, User, [$! | Cmd]}, #irc{data = Data}) ->
	{new_event, cmdevent, {chancmd, Chan, User, util:split(Cmd)}, dict:erase(Chan, Data)};
handle_event(genevent, {chanmsg, Chan, User, Msg}, Irc) ->
	AppealRE = io_lib:format("^\\s*~s[:, ](.*)", [Irc#irc.nick]), %"
	case re:run(Msg, AppealRE, [unicode, {capture, all_but_first, list}]) of
		{match, [Rest]} ->
			% Real appeal doesn't start anything (anyway most probably bot answers later)
			{new_event, msgevent, {appeal, Chan, User, Rest}, Irc#irc.data};
		nomatch ->
			check_genmsg(Chan, User, Msg, Irc)
	end;
handle_event(genevent, {action, Chan, User, Action}, Irc) ->
	check_genmsg(Chan, User, Action, Irc);
% Own channel message starts smart appeal. 
% @TODO Any other start causes?
handle_event(selfevent, {Cmd, Chan, _}, #irc{data = Data}) when Cmd =:= chanmsg; Cmd =:= action ->
	{ok, dict:store(Chan, {?MAX_LINES, erlang:now()}, Data)};
% Currently unused.
handle_event(customevent, {cancel_appeal, Chan}, #irc{data = Data}) ->
	{ok, dict:erase(Chan, Data)};
handle_event(_Type, _Event, _Irc) ->
	not_handled.

check_genmsg(Chan, User, Msg, #irc{data = Data}) ->
	{M2, S2, _} = Now = erlang:now(),
	case dict:find(Chan, Data) of
		error ->
			{new_event, msgevent, {genmsg, Chan, User, Msg}, Data};
		{ok, {0, _}} ->
			{new_event, msgevent, {genmsg, Chan, User, Msg}, dict:erase(Chan, Data)};
		% More than `APPEAL_TIMEOUT' seconds has passed since smart appeal started, cancel it
		{ok, {_, {M1, S1, _}}} when ((M2-M1)*1000000 + S2-S1) > ?APPEAL_TIMEOUT ->
			{new_event, msgevent, {genmsg, Chan, User, Msg}, dict:erase(Chan, Data)};
		{ok, {X, _}} ->
			check_maybe_appeal(Chan, User, Msg, dict:store(Chan, {X - 1, Now}, Data))
	end.

check_maybe_appeal(Chan, User, Msg, Data) ->
	AnyAppealRE = "^\\s*" ?NICK_REGEX ":.*",
	case re:run(Msg, AnyAppealRE, [unicode, {capture, none}]) of
		match ->
			%% If appeal to another user detected, cancel smart appeal
			{ok, dict:erase(Chan, Data)};
		nomatch ->
			{new_event, msgevent, {maybe_appeal, Chan, User, Msg}, Data}
	end.
