%%%-------------------------------------------------------------------
%%% File    : bhv_relay.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  9 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_relay).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

%% Data is dict: {Chan, Ident} -> [{SourceNick, Message}]
init(_) -> dict:new().

help(chancmd) ->
	[{"relay <ник или ~идент> <сообщение>", "передать сообщение пользователю, как только он зайдет на канал"}];
help(privcmd) ->
	none;
help(about) ->
	"Передача сообщений пользователям, которых в данный момент нет на канале".

handle_event(cmdevent, {chancmd, Chan, ?USER(Nick), ["relay", Target | Rest]}, _, Data) ->
	Msg = string:join(Rest, " "),
	queue_relay(Chan, Nick, Target, Msg, Data);
handle_event(chanevent, {join, Chan, ?USER2(Nick, Ident)}, _, Data) ->
	case dict:find({Chan, Ident}, Data) of
		error -> not_handled;
		{ok, Messages} -> relay_messages(Chan, Nick, Messages),
						  {ok, dict:erase({Chan, Ident}, Data)}
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

queue_relay(Chan, Nick, Target, Msg, Data) ->
	case to_ident(Chan, Target) of
		undefined -> bhv_common:empty_check(Chan, []);
		TargetIdent ->
			announce_relay(Chan, TargetIdent),
			{ok, dict:append({Chan, TargetIdent}, {Nick, Msg}, Data)}
	end.

announce_relay(Chan, TargetIdent) ->
	irc_conn:chanmsg(Chan, nohist, ["Передам ", TargetIdent, ", насяльника!"]).

to_ident(_, [$~ | _] = Ident) -> Ident;
to_ident(Chan, Nick) -> bhv_history:resolve_nick(Chan, Nick).

relay_messages(Chan, Nick, Messages) ->
	irc_conn:chanmsg(Chan, hist, [Nick, ": тебе просили передать"]),
	MsgList = [["<", Source, "> ", Msg] || {Source, Msg} <- Messages],
	irc_conn:bulk_chanmsg(Chan, hist, MsgList).
