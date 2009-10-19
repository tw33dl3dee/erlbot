%%%-------------------------------------------------------------------
%%% File    : bhv_google.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lojban).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

handle_event(cmdevent, {chancmd, Chan, _, ["Jbo" | Sentence]}, Irc) ->
	erlbot:jbofihe(Irc, Chan, string:join(Sentence, " ")),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["jbo" | Sentence]}, Irc) ->
	erlbot:cmafihe(Irc, Chan, string:join(Sentence, " ")),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["jvo" | Sentence]}, Irc) ->
	erlbot:jvocuhadju(Irc, Chan, string:join(Sentence, " ")),
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["en-jbo" | Words]}, Irc) ->
	[erlbot:dict(Irc, Chan, "www.lojban.org", "en->jbo", Word) || Word <- Words],
	{ok, undefined};
handle_event(cmdevent, {chancmd, Chan, _, ["jbo-en" | Words]}, Irc) ->
	[erlbot:dict(Irc, Chan, "www.lojban.org", "jbo->en", Word) || Word <- Words],
	{ok, undefined};
handle_event(_Type, _Event, _Irc) ->
	not_handled.
