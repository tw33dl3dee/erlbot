%%%-------------------------------------------------------------------
%%% File    : utf8.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Created : 15 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------

%%% @doc This module provides basic UTF8-handling routines and source "UTF-ication".
%%% For the latter, add `-compile([{parse_transform, utf8}])' to your source.
%%% All Latin1 string constants will be automatically replaced with Unicode strings.
%%% To keep some string constant as Latin1-encoded, use form 'LATIN':"latin1-string".

-module(utf8).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-compile([bin_opt_info]).

%% External API
-export([encode/1, decode/1, try_decode/1, split/2]).

%% parse_transform entry point
-export([parse_transform/2]).

%% @type charlist() = [unicode_char() | unicode_binary() | charlist()].
%%   A unicode_binary is allowed as the tail of the list
%% @type unicode_binary() = binary().
%%   Characters in binary are encoded in UTF-8 coding standard.
%% @type unicode_char() = integer().
%%   Represents valid unicode codepoint.

%% @doc Converts Unicode string to UTF8 binary
%% @spec encode(Text) -> Binary
%% where
%%    Text = charlist()
%%    Binary = unicode_binary()
encode(Text) ->
	unicode:characters_to_binary(Text).

%% @doc Converts UTF8 binary to Unicode string, skipping invalid bytes
%% @spec decode(Binary) -> Text
%% where
%%    Binary = unicode_binary()
%%    Text = charlist()
decode(Binary) ->
	case binary_to_characters(Binary, skip) of {utf8, Text} -> Text end.

%% @doc Tries to convert Latin1 string to Unicode string.
%%      Returns original string if conversion not possible.
%% @spec try_decode(Text) -> UtfText
%% where
%%    Text = string()
%%    UtfText = charlist() | string()
try_decode(Text) when is_list(Text) ->
	case binary_to_characters(list_to_binary(Text), abort) of
		{utf8, UtfText} -> UtfText;
		not_utf8 -> Text
	end.

binary_to_characters(Bin, OnError) ->
	binary_to_characters(Bin, OnError, []).

binary_to_characters(<<Char/utf8, Rest/binary>>, OnError, List) ->
	binary_to_characters(Rest, OnError, [Char | List]);
binary_to_characters(<<_Invalid:8, Rest/binary>>, skip, List) ->
	binary_to_characters(Rest, skip, List);
binary_to_characters(<<_Invalid:8, _Rest/binary>>, abort, _) ->
	not_utf8;
binary_to_characters(<<>>, _, List) ->
	{utf8, lists:reverse(List)}.

%% @doc Splits UTF8 binary into two valid UTF8 parts of specified size
%%   First part will have closest possible size to Len but not exceeding it.
%% @spec split(Binary, Len) -> {Head, Rest}
%% where
%%    Binary = unicode_binary()
%%    Head = unicode_binary()
%%    Rest = unicode_binary()
%%    Len = integer()
split(Binary, Len) when is_binary(Binary), Len > 1 ->
	split_once(Binary, Len).

split_once(Binary, Len) when size(Binary) =< Len ->
	{Binary, <<>>};
split_once(Binary, Len) ->
	{Head, Rest} = split_binary(Binary, Len),
	check_split(Head, Rest).

check_split(Head, Rest) ->
	case Rest of
		<<_/utf8, _/binary>> ->
			{Head, Rest};
		_ ->
			{NewHead, Tail} = split_binary(Head, size(Head) - 1),
			check_split(NewHead, <<Tail/binary, Rest/binary>>)
	end.

%% @private
%% @doc Transforms AST into new one, replacing Latin string constants with Unicode strings
%% @spec parse_transform(list(), list()) -> list()
parse_transform(Tree, _Options) ->
	%io:format("Tree: ~p, options: ~p~n", [Tree, _Options]),
	parse_node(Tree).

parse_node({remote, _, {atom, _, 'LATIN'}, {string, Line, LatinString}}) ->
	{string, Line, LatinString};
parse_node({string, Line, String}) ->
	{string, Line, try_decode(String)};
parse_node(Node) when is_list(Node) ->
	[parse_node(Elmt) || Elmt <- Node];
parse_node(Node) when is_tuple(Node) ->
	list_to_tuple([parse_node(Elmt) || Elmt <- tuple_to_list(Node)]);
parse_node(Node) ->
	Node.
