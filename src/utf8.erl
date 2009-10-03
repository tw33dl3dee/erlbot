-module(utf8).

-export([encode/1, decode/1, split/2]).

encode(Text) ->
	unicode:characters_to_binary(Text).

decode(Binary) ->
	binary_to_characters(Binary).

binary_to_characters(Bin) ->
	binary_to_characters(Bin, []).

binary_to_characters(<<Char/utf8, Rest/binary>>, List) ->
	binary_to_characters(Rest, [Char | List]);
binary_to_characters(<<_Invalid:8, Rest/binary>>, List) ->
	binary_to_characters(Rest, List);
binary_to_characters(<<>>, List) ->
	lists:reverse(List).

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
