%%%-------------------------------------------------------------------
%%% File    : bhv_lojban.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lojban).

-behaviour(irc_behaviour).
-export([init/1, help/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

init(_) -> undefined.

help(chancmd) -> 
	[{"Jbo <sentence>",			"трансляция с Ложбана"},
	 {"jbo <word>",				"разбор слова на Ложбане"},
	 {"jvo <word1> <word2>...",	"комбинирование слов в lujvo на Ложбане"},
	 {"en-jbo|jbo-en <word>",	"перевод слова с Ложбана"}];
help(privcmd) ->
	none;
help(about) ->
	"Перевод с Ложбана".

handle_event(cmdevent, {chancmd, Chan, _, ["Jbo" | Sentence]}, Irc) when length(Sentence) > 0 ->
	jbofihe(Irc, Chan, string:join(Sentence, " "));
handle_event(cmdevent, {chancmd, Chan, _, ["jbo" | Sentence]}, Irc) when length(Sentence) > 0 ->
	cmafihe(Irc, Chan, string:join(Sentence, " "));
handle_event(cmdevent, {chancmd, Chan, _, ["jvo" | Words]}, Irc) ->
	jvocuhadju(Irc, Chan, Words);
handle_event(cmdevent, {chancmd, Chan, _, ["en-jbo" | Words]}, Irc) ->
	[dict(Irc, Chan, "www.lojban.org", "en->jbo", Word) || Word <- Words],
	ok;
handle_event(cmdevent, {chancmd, Chan, _, ["jbo-en" | Words]}, Irc) ->
	[dict(Irc, Chan, "www.lojban.org", "jbo->en", Word) || Word <- Words],
	ok;
handle_event(_Type, _Event, _Irc) ->
	not_handled.

jbofihe(Irc, Chan, Sentence) ->
	{_, Lines} = util:system("head -n1 | jbofihe -x", [Sentence, $\n]),
	ok = irc_conn:async_chanmsg(Irc, Chan, Lines).

cmafihe(Irc, Chan, Sentence) ->
	{_, Lines} = util:system("head -n1 | cmafihe", [Sentence, $\n]),
	ok = irc_conn:async_chanmsg(Irc, Chan, Lines).

jvocuhadju(Irc, Chan, Words) ->
	{_, Lines} = util:execvp("jvocuhadju", Words),
	ok = irc_conn:async_chanmsg(Irc, Chan, Lines).

dict(Irc, Chan, Server, Db, Word) ->
	{success, Lines} = util:execv("dict.rb", ["-h", Server, "-d", Db, Word], ?SCRIPT_DIR),
	ok = irc_conn:async_chanmsg(Irc, Chan, bhv_common:empty_check(Lines)).
