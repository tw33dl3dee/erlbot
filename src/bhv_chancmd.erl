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

init(_) -> undefined.

handle_event(genevent, {chanmsg, Chan, User, [$! | Cmd]}, _Irc) ->
	{new_event, cmdevent, {chancmd, Chan, User, util:split(Cmd)}, undefined};
handle_event(genevent, {chanmsg, Chan, User, Msg}, Irc) ->
	AppealRE = io_lib:format("^[ \t]*~s[:, ](.*)", [Irc#irc.nick]), %"
	case re:run(Msg, AppealRE, [unicode, {capture, all_but_first, list}]) of
		{match, [Rest]} ->
			{new_event, customevent, {appeal, Chan, User, Rest}, undefined};
		nomatch ->
			{new_event, customevent, {genmsg, Chan, User, Msg}, undefined}
	end;
handle_event(genevent, {action, Chan, User, Action}, _Irc) ->
	{new_event, customevent, {genmsg, Chan, User, Action}, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
