%%%-------------------------------------------------------------------
%%% File    : bhv_appeal.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_appeal).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("irc.hrl").
-include("utf8.hrl").
-include("bhv_common.hrl").

-define(APPEAL_DELAY, 500).  % delay in ms between bot answer to appeal

init(_) -> undefined.

help(_) -> none.

handle_event(msgevent, {appeal, Chan, User, Msg}, Irc) ->
	handle_appeal(direct, Chan, User, Msg, Irc);
handle_event(msgevent, {maybe_appeal, Chan, User, Msg}, Irc) ->
	handle_appeal({indirect, chan}, Chan, User, Msg, Irc);
handle_event(cmdevent, {privcmd, ?USER(Nick) = User, Words}, Irc) ->
	handle_appeal({indirect, priv}, Nick, User, string:join(Words, " "), Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

%% Source may be user nickname or channel name
%% Cause = `direct' | {`indirect', `chan'} | {`indirect', `priv'}
handle_appeal(Cause, Source, ?USER(Nick) = User, Msg, Irc) ->
	AppealType = appeal_type(Msg),
	case appeal_react(AppealType, Source, Nick, Cause) of
		{message_react, Method, ReplyMsg} -> message_react(Irc, Source, Method, ReplyMsg);
		%% Induced `genmsg' is `customevent' which means that `msgevent' 
		%% (as `genmsg', `appeal' or `maybe_appeal') occurs once per user message
		not_handled when Cause =:= {indirect, chan} -> 
			{new_event, customevent, {genmsg, Source, User, Msg}, undefined};
		NewEvent -> NewEvent
	end.

-define(APPEAL_REGEX, [{humiliation, "(суч?ка|хуй|заткни)"},
					   {greeting,    "превед"},
					   {fuckoff,     "(у?ебись|сосн?и)"},
					   {caress,      "(няшка|кавай)"},
					   {criticism,   "(тупа+я +пи+зда+|пи+зда+ +тупа+я+)"},
					   {kiss,        "чмоки"}]).

appeal_type(Msg) -> appeal_type(?APPEAL_REGEX, Msg).

appeal_type([{Type, Regexp} | Rest], Msg) ->
	case util:contains(Msg, Regexp) of
		true  -> Type;
		false -> appeal_type(Rest, Msg)
	end;
appeal_type([], _) -> undefined.

appeal_react(kiss, _, _, _)             -> {message_react, action,  ["*KISSED* *YAHOO*"]};
appeal_react(humiliation, _, Nick, _)   -> {message_react, chanmsg, [Nick, ": хамишь, сцуко."]};
appeal_react(greeting, _, Nick, _)      -> {message_react, chanmsg, ["\\O/ Превед, ", Nick, "!!!"]};
appeal_react(caress, _, _, _)           -> {message_react, chanmsg, "^_^"};
appeal_react(criticism, _, _, _)        -> {message_react, action,  "тупая пизда v_v"};
appeal_react(fuckoff, Chan, Nick, _)
  when ?IS_CHAN(Chan)                   -> {delayed_event, ?APPEAL_DELAY, customevent, {suicide, Chan, Nick}, undefined};
appeal_react(_, _, _, direct)           -> {message_react, chanmsg, "Ня!"};
appeal_react(_, _Source, _Nick, _Cause) -> not_handled.

message_react(Irc, Source, Method, Msg) ->
	timer:sleep(?APPEAL_DELAY),
	ok = irc_conn:Method(Irc, Source, hist, Msg).
