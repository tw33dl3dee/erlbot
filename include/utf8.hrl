%-define(U(S), utf8:decode(list_to_binary(S))).

-compile([{parse_transform, utf8}]).
