%%%-------------------------------------------------------------------
%%% File    : bhv_timer.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  4 Apr 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_timer).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) ->
	[{"timer <относительное время (+HH:MM)> <сообщение>", "выдает через указанное время напоминание"},
	 {"timer <абсолютное время (HH:MM)> <сообщение>",     "выдает в указанное время дня напоминание"}];
help(privcmd) ->
	none;
help(about) ->
	"Таймеры для напоминания".

%% If bot is not present on channel at the moment of reminder, 
%% reschedule it for this timeout (msec).
-define(RESCHEDULE_TIMEOUT, 30000).

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["timer", Time | Rest]}, _, D) ->
	Message = string:join(Rest, " "),
	case parse_time(Time) of
		false ->
			not_handled;
		Timeout ->
			announce_timer(Chan, Nick, Timeout),
			{delayed_event, 1000*Timeout, customevent, {timer_expire, Chan, Nick, Message}, D}
	end;
handle_event(customevent, {timer_expire, Chan, Nick, Message}, _, D) ->
	case lists:member(Chan, irc_conn:get_channels()) of
		true ->
			irc_conn:chanmsg(Chan, hist, ["========== НАПОМИНАНИЕ от ", Nick, " =========="]),
			ok = irc_conn:chanmsg(Chan, hist, Message);
		false ->
			%% user not present, reschedule reminder
			{delayed_event, ?RESCHEDULE_TIMEOUT, customevent, {timer_expire, Chan, Nick, Message}, D}
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

%% relative time
parse_time([$+ | TimeSpec]) ->
	case parse_hhmm(TimeSpec) of 
		{HH, MM} -> erlbot_util:convert_time_rel_diff(HH, MM);
		false    -> false
	end;
%% absolute time
parse_time(TimeSpec) ->
	case parse_hhmm(TimeSpec) of 
		{HH, MM} -> case erlbot_util:convert_time_abs(HH, MM, tomorrow) of
						{time, DateTime} -> erlbot_util:time_diff(DateTime, erlang:universaltime());
						undefined ->        false
					end;
		false    -> false
	end.

parse_hhmm(TimeSpec) ->
	case re:run(TimeSpec, "^(\\d+)[:\\.](\\d\\d)", [unicode, {capture, all_but_first, list}]) of
		{match, [HH, MM]} ->
			{list_to_integer(HH), list_to_integer(MM)};
		nomatch ->
			false
	end.

announce_timer(Chan, _Nick, Timeout) ->
	{{Y, M, D}, {HH, MM, _}} = erlbot_util:add_seconds(erlang:localtime(), Timeout),
	HDiff = Timeout div 3600,
	MDiff = (Timeout div 60 rem 60),
	HPad = if HDiff >= 1000 -> 4;
			  HDiff >= 100  -> 3;
			  true          -> 2
		   end,
	Message = io_lib:format("Таймер установлен на ~2..0B:~2..0B ~2..0B/~2..0B/~2..0B"
							" (через ~*..0B:~2..0B), насяльника!",
							[HH, MM, Y rem 100, M, D, HPad, HDiff, MDiff]),
	ok = irc_conn:chanmsg(Chan, nohist, Message).
