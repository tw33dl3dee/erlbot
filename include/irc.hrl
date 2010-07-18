-define(USER(Nick), {Nick, _, _}).
-define(USER2(Nick, Ident), {Nick, Ident, _}).
-define(TOPIC(Topic), {Topic, _, _}).
-define(IS_CHAN(ChanName), hd(ChanName) =:= $# orelse hd(ChanName) =:= $& orelse hd(ChanName) =:= $+ orelse hd(ChanName) =:= $!).

-define(NICK_REGEX, "[" "a-z" "A-Z" "0-9" "_" "\\-" "\\[\\]" "\\\\" "`" "\\^" "{}" "]+").

%% This record is passed to all IRC behaviours
-record(irc, {nick              :: list(),     % current bot nick
			  login             :: list(),     % bot login
			  is_oper           :: boolean(),  % `true' if bot is server oper 
			  data = undefined  :: term()}).   % arbitrary data to store behaviour state
