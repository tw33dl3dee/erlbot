%%%-------------------------------------------------------------------
%%% File    : bhv_comment.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_comment).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(_) -> none.

handle_event(chanevent, {join, Chan, User}, _, _) ->
	make_comment(join, Chan, User);
handle_event(chanevent, {topic, Chan, User, Topic}, _, _) ->
	make_comment({topic, Topic}, Chan, User);
%% Bot can comment any `genmsg', direct or induced from `maybe_appeal'.
handle_event(_, {genmsg, Chan, User, Msg}, _, _) ->
	make_comment({message, Msg}, Chan, User);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

-define(COMMENT_REV_PROB, 50).  % 1/50th

-define(TOPIC_COMMENTS(Nick), [[neg, "Говенный топег, ", Nick, "."], 
							   [pos, "Гг :)"], 
							   {8, [pos, "Мощно задвинул, ", Nick, "."]}]).

-define(MESSAGE_COMMENTS(Nick), [[neg, "Хуйню спорол, ", Nick, "."], 
								 [pos, Nick, ": лови пиченьку."],
								 [neg, Nick, " -- дятел. ^_^"],
								 [pos, Nick, ", ты гений!"],
								 [pos, Nick, ": чмоки, противный"],
								 {3, ask}]).

make_comment(join, Chan, ?USER(Nick) = User) ->
	case bhv_common:is_bot(User) of
		false -> not_handled;
		%% Greet other bots
		true  -> irc_conn:chanmsg(Chan, hist, [Nick, ": няяяяяяяяя!"])
	end;
make_comment({topic, Topic}, Chan, ?USER(Nick)) ->
	make_emotional_comment(?TOPIC_COMMENTS(Nick), Chan, Nick, Topic);
make_comment({message, Msg}, Chan, ?USER(Nick)) ->
    case choice:make([{1, do}, {?COMMENT_REV_PROB - 1, dont}]) of
        do   -> make_emotional_comment(?MESSAGE_COMMENTS(Nick), Chan, Nick, Msg);
        dont -> not_handled
    end.

-define(SUICIDE_DISABLE_TIMEOUT, 120000).
-define(MIN_ASK_WORD_LEN, 4).  %% minimum word length that bot asks to repeat

make_emotional_comment(Alternatives, Chan, Nick, OrigMsg) ->
	case choice:make(Alternatives) of
		[pos | Msg] ->
			irc_conn:chanmsg(Chan, hist, Msg),
			{new_event, customevent, {suicide_enable, Nick}, undefined};
		[neg | Msg] ->
			irc_conn:chanmsg(Chan, hist, Msg),
			{new_event, customevent, {suicide_disable, Nick, ?SUICIDE_DISABLE_TIMEOUT}, undefined};
		ask ->
			case erlbot_util:words(OrigMsg, ?MIN_ASK_WORD_LEN) of
				[] -> not_handled;
				Words -> irc_conn:chanmsg(Chan, hist, [choice:make(Words), "?"])
			end
	end.
