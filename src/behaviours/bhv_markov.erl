%%%-------------------------------------------------------------------
%%% File    : bhv_markov.erl
%%% Author  : Ivan Korotkov <nkik@niisi.ras.ru>
%%% Description : 
%%%
%%% Created : 14 Jul 2010 by Ivan Korotkov <nkik@niisi.ras.ru>
%%%-------------------------------------------------------------------
-module(bhv_markov).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).
-export([update_wchain_text/1, gen_wchain/2]).

-include("utf8.hrl").
-include("irc.hrl").
-include("bhv_common.hrl").

-include_lib("stdlib/include/qlc.hrl").

-record(wchain, {words   :: tuple(),      %% prefix words + following word
				 freq    :: integer()}).  %% frequency (number of occurences)

-define(WCHAIN_PREFIX_LEN, 2).       %% word prefix length
-define(WCHAIN_PREFIX, W1, W2).      %% word prefix template
-define(WCHAIN_PREFIX_SHIFTED, W2).  %% word prefix template shifted by 1

init(_) -> 
	ok = erlbot_db:init_table(wchain, [{disc_copies, [node()]}, {attributes, record_info(fields, wchain)}]),
	undefined.

help(chancmd) ->
	[{"markov <первое слово>", "сгенерировать случайное предложение"}];
help(privcmd) ->
	[{"markov <первое слово>", "сгенерировать случайное предложение в приват"}];
help(about) ->
	"Генерация случайных предложений на основе цепей Маркова".

handle_event(customevent, {markov, Chan, Message}, _, _) ->
	case erlbot_util:words(Message, 1) of
		[]    -> not_handled;
		Words -> show_markov_sentence(lists:last(Words), Chan)
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["markov", Word]}, _, _) ->
	show_markov_sentence(Word, Chan);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["markov", Word]}, _, _) ->
	show_markov_sentence(Word, Nick);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

-define(WCHAIN_MAX_LEN, 10).

show_markov_sentence(Start, Target) ->
	case gen_wchain(Start, ?WCHAIN_MAX_LEN) of
		[] -> not_handled;
		Sentence ->
			Line = erlbot_util:join(" ", Sentence),
			ok = irc_conn:chanmsg(Target, hist, [Line, "."])
	end.

update_wchain_text(Text) ->
	Words = erlbot_util:words(Text, 1),
	ok = update_wchain(Words).

update_wchain([?WCHAIN_PREFIX, Wn | Words]) ->
	add_wchain({?WCHAIN_PREFIX, Wn}),
	update_wchain([?WCHAIN_PREFIX_SHIFTED, Wn | Words]);
update_wchain(_) ->
	ok.

add_wchain(Words) ->
	mnesia:dirty_update_counter(wchain, Words, 1).

%% Generate Markov chain of words by prefix
gen_wchain({?WCHAIN_PREFIX}, WordCount) -> gen_wchain(lists:reverse([?WCHAIN_PREFIX]), {?WCHAIN_PREFIX}, WordCount);
%% Generate Markov chain of words by first word (random prefix)
gen_wchain(W1, WordCount) -> 
	case random_prefix(W1) of
		none -> [];
		{?WCHAIN_PREFIX, _} -> 
			gen_wchain({?WCHAIN_PREFIX}, WordCount)
	end.

gen_wchain(Words, _, 0) -> lists:reverse(Words);
gen_wchain(Words, {?WCHAIN_PREFIX}, WordCount) ->
	case next_word({?WCHAIN_PREFIX}) of
		none     -> lists:reverse(Words);
		NextWord -> gen_wchain([NextWord | Words], {?WCHAIN_PREFIX_SHIFTED, NextWord}, WordCount - 1)
	end.

%% Randomly select following word by prefix
next_word({?WCHAIN_PREFIX}) ->
	case mnesia:dirty_select(wchain, [{#wchain{words = {?WCHAIN_PREFIX, '$1'}, freq = '$2'}, 
									   [], [{{'$2', '$1'}}]}]) of
		[]           -> none;
		Alternatives -> choice:make(Alternatives)
	end.

%% Random prefix by first word
random_prefix(W1) ->
	WchainPat = [list_to_atom([$$ | integer_to_list(X)]) || X <- lists:seq(2, ?WCHAIN_PREFIX_LEN + 1)],
	MatchPat = list_to_tuple([W1 | WchainPat]),
	ResPat = {{'$1', {list_to_tuple([W1 | WchainPat])}}},
	case mnesia:dirty_select(wchain, [{#wchain{words = MatchPat, freq = '$1'}, [], [ResPat]}]) of
		[]           -> none;
		Alternatives -> choice:make(Alternatives)
	end.
