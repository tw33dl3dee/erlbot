%%%-------------------------------------------------------------------
%%% File    : bhv_pom.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_pom).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> 
	["!mooon : текущая фаза Луны", 
	 "!moooon : даты ближайших чертвертей фазы Луны"].

handle_event(cmdevent, {chancmd, Chan, _, ["mooon"]}, Irc) ->
	{success, [Moon]} = util:system("pom"),
	ok = irc_conn:chanmsg(Irc, Chan, Moon);
handle_event(cmdevent, {chancmd, Chan, _, ["moooon"]}, Irc) ->
	% Emacs power!
	Cmd = "emacs --batch -Q --eval '(progn (lunar-phases) (with-current-buffer  \"*Phases of Moon*\" (message (buffer-string))))' "
		"2>&1 | grep -v ^Computing | tail -n +5 | head -n 8",
	{success, Lines} = util:system(Cmd),
	ok = irc_conn:async_chanmsg(Irc, Chan, Lines);
handle_event(_Type, _Event, _Irc) ->
	not_handled.
