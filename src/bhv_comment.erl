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
	comment(topic, Chan, Nick, Irc);
%% Bot can comment any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, ?USER(Nick), _}, Irc) ->
	comment(message, Chan, Nick, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

-define(COMMENT_REV_PROB, 50).  % 1/50th

%% Some of these are unused

comment(topic, Chan, Nick, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, nohist, choice:make([["Говенный топег, ", Nick, "."], 
														  "Гг :)", 
														  {8, ["Мощно задвинул, ", Nick, "."]}]));
comment(message, Chan, Nick, Irc) ->
    case choice:make([{1, do}, {?COMMENT_REV_PROB - 1, dont}]) of
        do ->
			case choice:make([[neg, "Хуйню спорол, ", Nick, "."], 
							  [pos, Nick, ": лови пиченьку."],
							  [neg, Nick, " -- дятел. ^_^"],
							  [pos, Nick, ", ты гений!"],
							  [pos, Nick, ": чмоки, противный"]]) of
				[Emotion | Msg] ->
					comment(Emotion, Chan, Nick, Msg, Irc)
			end;
        dont ->
            ok
    end.

-define(SUICIDE_DISABLE_TIMEOUT, 120000).

comment(pos, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, hist, Msg),
	{new_event, customevent, {suicide_enable, Nick}, undefined};
comment(neg, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, hist, Msg),
	{new_event, customevent, {suicide_disable, Nick, ?SUICIDE_DISABLE_TIMEOUT}, undefined}.
