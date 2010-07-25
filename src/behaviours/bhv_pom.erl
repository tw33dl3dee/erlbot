%%%-------------------------------------------------------------------
%%% File    : bhv_pom.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_pom).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"mooon",	"текущая фаза Луны"},
	 {"moooon",	"даты ближайших чертвертей фазы Луны"}];
help(privcmd) ->
	none;
help(about) ->
	"Фазы Луны".

handle_event(cmdevent, {chancmd, Chan, _, ["mooon"]}, _, _) ->
	{success, [Moon]} = erlbot_util:system("pom"),
	ok = irc_conn:chanmsg(Chan, hist, Moon);
handle_event(cmdevent, {chancmd, Chan, _, ["moooon"]}, _, _) ->
	% Emacs power!
	Cmd = "emacs --batch -Q --eval '(progn (lunar-phases) (with-current-buffer  \"*Phases of Moon*\" (message (buffer-string))))' "
		"2>&1 | grep -v ^Computing | tail -n +5 | head -n 8",
	{success, Lines} = erlbot_util:system(Cmd),
	ok = irc_conn:bulk_chanmsg(Chan, hist, Lines);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.
