%%%-------------------------------------------------------------------
%%% File    : bhv_comment.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_comment).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

handle_event(chanevent, {topic, Chan, ?USER(Nick), _}, Irc) ->
	comment(topic, Chan, Nick, Irc);
handle_event(chanevent, {join, Chan, ?USER(Nick)}, Irc) ->
	comment(join, Chan, Nick, Irc);
handle_event(chanevent, {part, Chan, ?USER(Nick), _}, Irc) ->
	comment(exit, Chan, Nick, Irc);
handle_event(chanevent, {quit, Chan, ?USER(Nick), _}, Irc) ->
	comment(exit, Chan, Nick, Irc);
%% Bot can comment any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, ?USER(Nick), _}, Irc) ->
	comment(message, Chan, Nick, Irc);
handle_event(_Type, _Event, _Irc) ->
	not_handled.

-define(COMMENT_REV_PROB, 50).  % 1/50th

comment(topic, Chan, Nick, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, choice:make([["Говенный топег, ", Nick, "."], 
												  "Гг :)", 
												  {8, ["Мощно задвинул, ", Nick, "."]}]));
comment(join, Chan, Nick, Irc) ->
	ok = irc_conn:command(Irc, choice:make([{2, {chanmsg, Chan, ["Превед, ", Nick, "."]}}, 
											{chanmsg, Chan, [">> ВНИМАНИЕ: К нам приходит пользователь СИСЬКИ^W", Nick, 
															 ". Поприветствуем!"]},	
											{action, Chan, ["приветствует ", Nick, "."]}]));
comment(exit, Chan, Nick, Irc) ->
	ok = irc_conn:chanmsg(Irc, Chan, choice:make([["Нам будет нехватать тебя, ", Nick, "."], 
												  "Гг, наконец-то он ушел."]));
comment(message, Chan, Nick, Irc) ->
    case choice:make([{1, do}, {?COMMENT_REV_PROB - 1, dont}]) of
        do ->
			case choice:make([[neg, "Хуйню спорол, ", Nick, "."], 
							  [pos, Nick, ": лови пиченьку."],
							  [neg, Nick, " -- дятел. ^_^"],
							  [pos, Nick, ", ты гений!"]]) of
				[Emotion | Msg] ->
					comment(Emotion, Chan, Nick, Msg, Irc)
			end;
        dont ->
            ok
    end.

-define(SUICIDE_DISABLE_TIMEOUT, 120000).

comment(pos, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Msg),
	{new_event, customevent, {suicide_enable, Nick}, undefined};
comment(neg, Chan, Nick, Msg, Irc) ->
	irc_conn:chanmsg(Irc, Chan, Msg),
	{new_event, customevent, {suicide_disable, Nick, ?SUICIDE_DISABLE_TIMEOUT}, undefined}.
