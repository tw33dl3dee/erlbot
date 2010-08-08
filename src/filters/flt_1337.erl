%%%-------------------------------------------------------------------
%%% File    : flt_1337.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  8 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(flt_1337).

-behaviour(erlbot_filter).
-export([init/1, filter_command/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

-define(LEETSPEAK_REV_PROB, 100).  %% Chance of message being translated into 1337 speak: 1/100th

filter_command({Type, Chan, Save, Msg}, _IrcState, _Data) when Type =:= chanmsg; Type =:= action ->
	case choice:make([{1, do}, {?LEETSPEAK_REV_PROB - 1, dont}]) of
        dont -> not_handled;
        do   -> {new_command, {Type, Chan, Save, to_leetspeak(Msg)}}
    end;
filter_command(_Command, _IrcState, _Data) ->
	not_handled.

to_leetspeak(S) -> [choice:make(leet_letters(C)) || C <- lists:flatten(S)].

%% ph|_|{|< ^^j 8|24!/\/2, (V)@|\|

leet_letters($a) -> [$4, $@, "/-\\", "/\\", $^, "aye", "∂", "ci", "λ", $Z];
leet_letters($b) -> [$8, "|3", $6, "13", "|3", "ß", "]3"];
leet_letters($c) -> [$(, $<, "¢", ${, "©", "sea", "see"];
leet_letters($d) -> ["|)", "[)", "∂", "])", "I>", "|>", $0, "ð", "cl"];
leet_letters($e) -> [$3, "£", $&, "€", "[-", "ə"];
leet_letters($f) -> ["|=", "]=", $}, "ph", "(=", "ʃ"];
leet_letters($g) -> [$6, $9, $&, "(_+", "C-", "gee", "jee", "(γ", "cj"];
leet_letters($h) -> ["|-|", $#, "]-[", "[-]", ")-(", "(-)", ":-:", "}{", "}-{", $#, "aych"];
leet_letters($i) -> [$!, $1, $|, "eye", "3y3", "ai", "¡"];
leet_letters($j) -> ["_|", "_/", $], "¿", "</", "_)", "ʝ"];
leet_letters($k) -> [$X, "|<", "|X", "|{", "ɮ"];
leet_letters($l) -> [$1, $7, "|_", "£", "$|", "|_", "lJ", "¬"];
leet_letters($m) -> ["/\\/\\", "|\\/|", "em", "|v|", "[V]", "^^", "nn", "(V)", "(\\/)", "/|\\", "/|/|", ".\\\\", "/^^\\", "/V\\", "|^^|"];
leet_letters($n) -> ["|\\|", "/\\/", "[\\]", "<\\>", "{\\}", "//", "₪", "[]\\[]", "]\\[", $~];
leet_letters($o) -> [$0, "()", "oh", "[]", "¤", "Ω"];
leet_letters($p) -> ["|*", "|o", "|º", "|>", "|\"", $?, $9, "[]D", "|7", $q, "þ", "¶", "℗", "|D"];
leet_letters($q) -> ["0_", "0,", "(,)", "<|", "cue", $9, "¶"];
leet_letters($r) -> ["|2", $2, "/2", "I2", "|^", "|~", "lz", "®", "|2", "[z", "|`", "l2", "Я", ".-", "ʁ"];
leet_letters($s) -> [$5, $$, $z, "§", "es"];
leet_letters($t) -> [$7, $+, "-|-", $1, "']['", "†"];
leet_letters($u) -> ["|_|", "(_)", "Y3W", $M, "µ", "[_]", "\\_/", "\\_\\", "/_/"];
leet_letters($v) -> ["\\/", "√", "\\\\//"];
leet_letters($w) -> ["\\/\\/", "vv", "\'//", "\\\\\'", "\\^/", "(n)", "\\X/", "\\|/", "\\_|_/", "\\_:_/", "]I[", "UU", "Ш", "ɰ"];
leet_letters($x) -> [$%, "><", "Ж", "}{", "ecks", "×", $*, ")(", "ex"];
leet_letters($y) -> [$j, "`/", "`(", "-/", "\'/", "Ψ", "φ", "λ", "Ч", "¥"];
leet_letters($z) -> [$2, "≥", "~/_", $%, "ʒ", "7_"];
leet_letters(C) when C >= $A, C =< $Z -> 
	leet_letters(erlbot_util:lc(C));
leet_letters(C) -> 
	[C].
