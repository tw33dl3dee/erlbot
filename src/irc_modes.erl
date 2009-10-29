%%% File    : irc_modes.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%% Created : 29 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>

-module(irc_modes).

-export([umode_to_atom/1, atom_to_umode/1]).

umode_to_atom($i) -> invisible;
umode_to_atom($d) -> debug;
umode_to_atom($y) -> spy;
umode_to_atom($s) -> servnotices;
umode_to_atom($c) -> connnotices;
umode_to_atom($f) -> floodnotices;
umode_to_atom($w) -> wallops;
umode_to_atom($b) -> chatops;
umode_to_atom($o) -> globoper;
umode_to_atom($O) -> locoper;
umode_to_atom($A) -> servadmin;
umode_to_atom($a) -> serviceadmin;
umode_to_atom($F) -> nofloodlimits;
umode_to_atom(_)  -> undefined.

atom_to_umode(invisible) -> $i;
atom_to_umode(debug) -> $d;
atom_to_umode(spy) -> $y;
atom_to_umode(servnotices) -> $s;
atom_to_umode(connnotices) -> $c;
atom_to_umode(floodnotices) -> $f;
atom_to_umode(wallops) -> $w;
atom_to_umode(chatops) -> $b;
atom_to_umode(globoper) -> $o;
atom_to_umode(locoper) -> $O;
atom_to_umode(servadmin) -> $A;
atom_to_umode(serviceadmin) -> $a;
atom_to_umode(nofloodlimits) -> $F.
