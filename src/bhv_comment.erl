%%%-------------------------------------------------------------------
%%% File    : bhv_comment.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_comment).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(chanevent, {topic, Chan, ?USER(Nick), _}, Irc) ->
	make_comment(topic, Chan, Nick, Irc);
%% Bot can comment any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, ?USER(Nick), _}, Irc) ->
	make_comment(message, Chan, Nick, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

-define(COMMENT_REV_PROB, 50).  % 1/50th

-define(TOPIC_COMMENTS(Nick), [[neg, "Говенный топег, ", Nick, "."], 
							   [pos, "Гг :)"], 
							   {8, [pos, "Мощно задвинул, ", Nick, "."]}]).

-define(MESSAGE_COMMENTS(Nick), [[neg, "Хуйню спорол, ", Nick, "."], 
								 [pos, Nick, ": лови пиченьку."],
								 [neg, Nick, " -- дятел. ^_^"],
								 [pos, Nick, ", ты гений!"],
								 [pos, Nick, ": чмоки, противный"]]).

make_comment(topic, Chan, Nick, Irc) ->
	make_comment(?TOPIC_COMMENTS(Nick), Chan, Nick, Irc);
make_comment(message, Chan, Nick, Irc) ->
    case choice:make([{1, do}, {?COMMENT_REV_PROB - 1, dont}]) of
        do   -> make_comment(?MESSAGE_COMMENTS(Nick), Chan, Nick, Irc);
        dont -> ok
    end;
make_comment(Alternatives, Chan, Nick, Irc) ->
	case choice:make(Alternatives) of
		[Emotion | Msg] ->
			make_emotional_comment(Emotion, Chan, Nick, Msg, Irc)
	end.

-define(SUICIDE_DISABLE_TIMEOUT, 120000).

make_emotional_comment(pos, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, hist, Msg),
	{new_event, customevent, {suicide_enable, Nick}, undefined};
make_emotional_comment(neg, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, hist, Msg),
	{new_event, customevent, {suicide_disable, Nick, ?SUICIDE_DISABLE_TIMEOUT}, undefined}.
