%-define(U(S), utf8:decode(list_to_binary(S))).

-ifndef(NO_UTF8_TRANSFORM).
-compile([{parse_transform, utf8}]).
-endif.

%% Special code-point inserted into strings where original UTF8 contains invalid byte sequence
-define(UTF8_INVALID_SEQ, 2).
