%% events that are propagated to channel FSM (channel name must be 2nd element in event tuple)
-define(CHAN_EVENTS, [joining, chantopic, names, endofnames, parted, kicked, join, part, kick, mode, mymode, topic, mytopic]).

%% events that are propagated to ALL channel FSMs
-define(ALL_CHAN_EVENTS, [quit, nick]).

%% events that are NOT propagated back to irc_conn handlers
-define(HIDDEN_CHAN_EVENTS, [joining, chantopic, names]).

%% events that are sent to channel FSM synchronously
-define(SYNC_CHAN_EVENTS, [endofnames]).
