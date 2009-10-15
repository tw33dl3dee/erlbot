-module(utf8).

-export([encode/1, decode/1, try_decode/1, split/2, parse_transform/2, format_error/1]).

encode(Text) ->
	unicode:characters_to_binary(Text).

decode(Binary) ->
	case binary_to_characters(Binary, skip) of {utf8, Text} -> Text end.

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

try_decode(Text) when is_list(Text) ->
	case binary_to_characters(list_to_binary(Text), abort) of
		{utf8, UtfText} -> UtfText;
		not_utf8 -> Text
	end.

split(Binary, Len) when is_binary(Binary), Len > 1 ->
	split_once(Binary, Len).

%% split(<<>>, _, Result) ->
%% 	lists:reverse(Result);
%% split(Binary, Len, Result) ->
%% 	{Head, Rest} = split_once(Binary, Len),
%% 	split(Rest, Len, [Head | Result]).

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

parse_transform(Tree, _Options) ->
	io:format("Tree: ~p, options: ~p~n", [Tree, _Options]),
	Tree2 = induce_errors(parse_node(Tree)),
	io:format("New tree: ~p~n", [Tree2]),
	Tree2.

parse_node({remote, _, {atom, _, 'CHAR'}, {atom, Line, Char}}) ->
	case try_decode(atom_to_list(Char)) of
		[CP] -> {char, Line, CP};
		_Err -> put_error(Line, ["not a Unicode character: ~ts", [Char]])
	end;
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

-define(ERR_KEY, utf8_transform_errors).

put_error(Line, What) ->
	Error = {error, {Line, ?MODULE, What}},
	Errors = case get(?ERR_KEY) of
				 undefined -> [Error];
				 List      -> [Error | List]
			 end,
	put(?ERR_KEY, Errors),
	{atom, Line, error_here}.

induce_errors(Tree) ->
	case get(?ERR_KEY) of
		undefined -> Tree;
		Errors    -> erase(?ERR_KEY), Errors ++ Tree
	end.

format_error([Fmt, Args]) ->
	io_lib:format(Fmt, Args).
