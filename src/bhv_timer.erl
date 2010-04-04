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
	[{"timer <время (mm:ss)> <сообщение>", "выдает через указанное время напоминание"}];
help(privcmd) ->
	none;
help(about) ->
	"Таймеры для напоминания".

handle_event(cmdevent, {chancmd, Chan, _, ["timer", Time | Rest]}, _Irc) ->
	Message = string:join(Rest, " "),
	case parse_time(Time) of
		false ->
			not_handled;
		Timeout ->
			{delayed_event, 1000*Timeout, customevent, {timer_expire, Chan, Message}, undefined}
	end;
handle_event(customevent, {timer_expire, Chan, Message}, Irc) ->
	irc_conn:chanmsg(Irc, Chan, "========== НАПОМИНАНИЕ =========="),
	ok = irc_conn:chanmsg(Irc, Chan, Message);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

parse_time(TimeSpec) ->
	case re:run(TimeSpec, "^(\\d?\\d)[:\\.](\\d\\d)", [unicode, {capture, all_but_first, list}]) of
		{match, [MM, SS]} ->
			60*list_to_integer(MM) + list_to_integer(SS);
		nomatch ->
			false
	end.
