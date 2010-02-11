%%% File    : irc_modes.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%% Created : 29 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>

-module(irc_modes).

-export([umode_to_atom/1, atom_to_umode/1]).

umode_to_atom($i) -> invisible;
umode_to_atom($s) -> servnotices;
umode_to_atom($w) -> wallops;
umode_to_atom($o) -> oper;
umode_to_atom($O) -> helper;
umode_to_atom($a) -> admin;
umode_to_atom($F) -> nofloodlimits;
umode_to_atom(_)  -> undefined.

atom_to_umode(invisible) -> $i;
atom_to_umode(servnotices) -> $s;
atom_to_umode(wallops) -> $w;
atom_to_umode(oper) -> $o;
atom_to_umode(helper) -> $O;
atom_to_umode(admin) -> $a;
atom_to_umode(nofloodlimits) -> $F.
