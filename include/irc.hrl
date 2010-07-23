-define(USER(Nick), {Nick, _, _}).
-define(USER2(Nick, Ident), {Nick, Ident, _}).
-define(TOPIC(Topic), {Topic, _, _}).
-define(IS_CHAN(ChanName), hd(ChanName) =:= $# orelse hd(ChanName) =:= $& orelse hd(ChanName) =:= $+ orelse hd(ChanName) =:= $!).

-define(NICK_REGEX, "[" "a-z" "A-Z" "0-9" "_" "\\-" "\\[\\]" "\\\\" "`" "\\^" "{}" "]+").

%% Common data that is passed to all behaviours
-record(irc_state, {nick         :: list(),		  % current bot nick
					login        :: list(),		  % bot login
					is_servop    :: boolean(),    % `true' if bot is server oper 
					umode        :: [atom()]}).   % list of umodes
					
