-define(USER(Nick), {Nick, _, _}).
-define(TOPIC(Topic), {Topic, _, _}).
-define(IS_CHAN(ChanName), hd(ChanName) =:= $# orelse hd(ChanName) =:= $& orelse hd(ChanName) =:= $+ orelse hd(ChanName) =:= $!).

%% This record is passed to all IRC behaviours
-record(irc, {nick              :: list(),     % current bot nick
			  login             :: list(),     % bot login
			  is_oper           :: boolean(),  % `true' if bot is server oper 
			  conn_ref          :: pid(),      % `irc_conn' process pid
			  data = undefined  :: term()}).   % arbitrary data to store behaviour state
