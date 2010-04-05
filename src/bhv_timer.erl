%%%-------------------------------------------------------------------
%%% File    : bhv_timer.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  4 Apr 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_timer).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

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

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["timer", Time | Rest]}, Irc) ->
	Message = string:join(Rest, " "),
	case parse_time(Time) of
		false ->
			not_handled;
		Timeout ->
			announce_timer(Chan, Nick, Timeout, Irc),
			{delayed_event, 1000*Timeout, customevent, {timer_expire, Chan, Nick, Message}, undefined}
	end;
handle_event(customevent, {timer_expire, Chan, Nick, Message}, Irc) ->
	case lists:member(Chan, irc_conn:get_channels(Irc)) of
		true ->
			irc_conn:chanmsg(Irc, Chan, ["========== НАПОМИНАНИЕ от ", Nick, " =========="]),
			ok = irc_conn:chanmsg(Irc, Chan, Message);
		false ->
			%% user not present, reschedule reminder
			{delayed_event, ?RESCHEDULE_TIMEOUT, customevent, {timer_expire, Chan, Nick, Message}, undefined}
	end;
handle_event(_Type, _Event, _Irc) ->
	not_handled.

%% relative time
parse_time([$+ | TimeSpec]) ->
	case parse_hhmm(TimeSpec) of 
		{HH, MM} -> util:convert_time_rel_diff(HH, MM);
		false    -> false
	end;
%% absolute time
parse_time(TimeSpec) ->
	case parse_hhmm(TimeSpec) of 
		{HH, MM} -> case util:convert_time_abs(HH, MM, tomorrow) of
						{time, DateTime} ->
							util:time_diff(DateTime, erlang:universaltime());
						undefined ->
							false
					end;
		false    -> false
	end.

parse_hhmm(TimeSpec) ->
	case re:run(TimeSpec, "^(\\d?\\d)[:\\.](\\d\\d)", [unicode, {capture, all_but_first, list}]) of
		{match, [HH, MM]} ->
			{list_to_integer(HH), list_to_integer(MM)};
		nomatch ->
			false
	end.

announce_timer(Chan, _Nick, Timeout, Irc) ->
	{{Y, M, D}, {HH, MM, _}} = util:add_seconds(erlang:localtime(), Timeout),
	Message = io_lib:format("Таймер установлен на ~2..0B:~2..0B ~2..0B/~2..0B/~2..0B"
							" (через ~2..0B:~2..0B), насяльника!",
							[HH, MM, Y rem 100, M, D, Timeout div 3600, (Timeout div 60 rem 60)]),
	ok = irc_conn:chanmsg(Irc, Chan, Message).
