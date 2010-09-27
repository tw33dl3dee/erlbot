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
-export([sources/0, add_source/2, remove_source/1]).

-include("utf8.hrl").
-include("irc.hrl").

-define(PREFIX_MAX_LEN, 2).       %% word prefix length

init(_) -> undefined.

help(chancmd) ->
	[{"markov <первое слово>", "сгенерировать случайное предложение"}];
help(privcmd) ->
	[{"markov <первое слово>", "сгенерировать случайное предложение в приват"}];
help(about) ->
	"Генерация случайных предложений на основе цепей Маркова".

handle_event(genevent, {chanmsg, Chan, _, Msg}, _, _) ->
	update_wchains_from_text(Msg, ["history:", Chan]);
handle_event(genevent, {action, Chan, _, Msg}, _, _) ->
	update_wchains_from_text(Msg, ["history:", Chan]);
handle_event(customevent, {markov, Chan, Message}, _, _) ->
	case erlbot_util:words(Message, 1) of
		[]    -> not_handled;
		Words -> show_markov_sentence([lists:last(Words)], Chan)
	end;
handle_event(cmdevent, {chancmd, Chan, _, ["markov", Word]}, _, _) ->
	show_markov_sentence([Word], Chan);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["markov", Word]}, _, _) ->
	show_markov_sentence([Word], Nick);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

%%% Update

update_wchains_from_text(Text, Source) ->
	Words = erlbot_util:words(Text, 1),
	update_wchains(Words, Source).

update_wchains(Words, Source) when length(Words) > ?PREFIX_MAX_LEN ->
	add_wchain(lists:sublist(Words, ?PREFIX_MAX_LEN + 1), Source),
	update_wchains(tl(Words), Source);
update_wchains(_, _) ->
	ok.

add_wchain(Wchain, Source) ->
	couchbeam_db:save_doc(erlbot_db, wchain_to_json(Wchain, Source)).

wchain_to_json(Wchain, Source) ->
	{_, Ts} = erlbot_util:unix_timestamp(),
	{[{<<"_id">>,  list_to_binary(["markov:", Ts])},
	  {source,     utf8:encode(Source)},
	  {word_chain, [utf8:encode(W) || W <- Wchain]}]}.

%%% Show

-define(WCHAIN_MAX_LEN, 20).

show_markov_sentence(Start, Target) ->
	case generate_text(Start, ?WCHAIN_MAX_LEN) of
		[] -> not_handled;
		Sentence ->
			Line = erlbot_util:join(" ", Sentence),
			irc_conn:chanmsg(Target, hist, Line)
	end.

%% Generate Markov chain of words by starting prefix
generate_text(Start, MaxLen) ->
	case random_wchain([utf8:encode(W) || W <- Start], forward) of
		none   -> [];
		Prefix -> FwText = continue_text(lists:reverse(Prefix), forward, MaxLen),
                  %% backward chain may be no longer than forward chain
                  BwMaxLen = erlang:min(MaxLen - length(FwText), length(FwText)),
				  BwText = continue_text(Prefix, reverse, BwMaxLen),
				  BwText ++ lists:nthtail(length(Prefix), FwText)
	end.

continue_text(Text, forward, 0) -> 
	lists:reverse(Text);
continue_text(Text, reverse, 0) -> 
	Text;
continue_text(Text, Direction, MaxLen) ->
	Prefix = lists:sublist(Text, ?PREFIX_MAX_LEN),
	case random_wchain(lists:reverse(Prefix), Direction) of
		none   -> continue_text(Text, Direction, 0);
		Wchain -> continue_text([lists:last(Wchain) | Text], Direction, MaxLen - 1)
	end.

%% Randomly select word chain by prefix
random_wchain(Prefix, Direction) ->
	case erlbot_db:query_view({"markov2", view_name(Direction)},
							  [{startkey, Prefix}, 
							   {endkey, Prefix ++ [{[]}]}, 
							   {group, true}]) of
		{_, _, _, Chains} when length(Chains) > 0 ->
			Alternatives = [{Weight, Chain} || {_, Chain, Weight} <- Chains],
			choice:make(Alternatives);
		_ -> none
	end.

view_name(forward) -> "wchain";
view_name(reverse) -> "wchain_rev".

%%% Sources control

add_source(_Name, _File) ->
	ok.

%% BUG: implement deletion by id
remove_source(Name) ->
	NameBin = utf8:encode(Name),
	erlbot_db:foldl_view(fun ({_DocId, _, _, Doc}, S) -> couchbeam_db:delete_doc(erlbot_db, Doc), 
														 S + 1 
						 end, 0,
						 {"markov", "sources"}, [{startkey, NameBin}, {endkey, NameBin}, 
												 {reduce, false}, {include_docs, true}]).

sources() ->
	{_, _, _, Sources} = erlbot_db:query_view({"markov", "sources"}, [{group, true}]),
	[{utf8:decode(Name), Count} || {_, Name, Count} <- Sources].

%% Generate word chain statistic from history
upload_history(Chan, Ident) ->
	erlbot_db:foldl_view(fun ({_, [_, _, Ident1], _Msg}, S) when Ident =:= Ident1 -> 
								 io:format("SKIP ~ts~n", [_Msg]), S;
							 ({_, [Chan1 | _], Msg}, S) when Chan =:= Chan1 -> 
								 io:format("ADD  ~ts~n", [Msg]),
								 update_wchains_from_text(Msg, ["history:", Chan]), S + 1;
							 (_, S) -> S
						 end, 0, 
						 {"history", "msg_by_chan"}, []).
