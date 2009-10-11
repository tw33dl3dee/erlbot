-define(USER(Nick), {Nick, _, _}).
-define(TOPIC(Topic), {Topic, _, _}).
-define(IS_CHAN(ChanName), hd(ChanName) =:= $# orelse hd(ChanName) =:= $& orelse hd(ChanName) =:= $+ orelse hd(ChanName) =:= $!).

%% This record is passed to all IRC behaviours
-record(irc, {nick      :: list(),
			  login     :: list(),
			  is_oper   :: boolean(),
			  conn_ref  :: pid()}).
