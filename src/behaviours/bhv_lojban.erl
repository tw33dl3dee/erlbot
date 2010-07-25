%%%-------------------------------------------------------------------
%%% File    : bhv_lojban.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_lojban).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

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

handle_event(cmdevent, {chancmd, Chan, _, ["Jbo" | Sentence]}, _, _) when length(Sentence) > 0 ->
	jbofihe(Chan, string:join(Sentence, " "));
handle_event(cmdevent, {chancmd, Chan, _, ["jbo" | Sentence]}, _, _) when length(Sentence) > 0 ->
	cmafihe(Chan, string:join(Sentence, " "));
handle_event(cmdevent, {chancmd, Chan, _, ["jvo" | Words]}, _, _) ->
	jvocuhadju(Chan, Words);
handle_event(cmdevent, {chancmd, Chan, _, ["en-jbo" | Words]}, _, _) ->
	[dict(Chan, "www.lojban.org", "en->jbo", Word) || Word <- Words],
	ok;
handle_event(cmdevent, {chancmd, Chan, _, ["jbo-en" | Words]}, _, _) ->
	[dict(Chan, "www.lojban.org", "jbo->en", Word) || Word <- Words],
	ok;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

jbofihe(Chan, Sentence) ->
	{_, Lines} = erlbot_util:system("head -n1 | jbofihe -x", [Sentence, $\n]),
	ok = irc_conn:bulk_chanmsg(Chan, hist, Lines).

cmafihe(Chan, Sentence) ->
	{_, Lines} = erlbot_util:system("head -n1 | cmafihe", [Sentence, $\n]),
	ok = irc_conn:bulk_chanmsg(Chan, hist, Lines).

jvocuhadju(Chan, Words) ->
	{_, Lines} = erlbot_util:execvp("jvocuhadju", Words),
	ok = irc_conn:bulk_chanmsg(Chan, hist, Lines).

dict(Chan, Server, Db, Word) ->
	ok = bhv_common:pipe_script(Chan, "dict.rb", ["-h", Server, "-d", Db, Word]).
