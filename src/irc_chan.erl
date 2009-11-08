-module(irc_chan).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%%% gen_fsm
-behaviour(gen_fsm).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).
-export([state_joining/2, state_topic/2, state_names/2, state_names/3, state_joined/2, state_joined/3]).

%%% public interface
-export([start/1, start_link/1, chan_event/2, get_chan_info/1]).

-record(chan, {name                 :: list(),
			   topic = {[], [], 0}  :: {list(), list(), integer()},   %% topic, author, ts
			   users = []           :: [{atom(), list(), list()}]}).  %% type (op/user), nick, flags

-include("irc.hrl").

%% events that are NOT propagated back to irc_conn handlers (`noevent' reply is sent back)
-define(IS_HIDDEN_EVENT(Event), 
		element(1, Event) =:= joining;
		element(1, Event) =:= chantopic;
		element(1, Event) =:= names).

%% events that are sent to channel FSM synchronously
-define(IS_SYNC_EVENT(Event), 
		element(1, Event) =:= endofnames;
		element(1, Event) =:= quit;
		element(1, Event) =:= nick).

%%% Public interface

start(Name) ->
	gen_fsm:start(?MODULE, Name, []).

start_link(Name) ->
	gen_fsm:start_link(?MODULE, Name, []).

chan_event(FsmRef, Event) when ?IS_SYNC_EVENT(Event) ->
	gen_fsm:sync_send_event(FsmRef, Event, infinity);
chan_event(FsmRef, Event) ->
	gen_fsm:send_event(FsmRef, Event),
	chan_event_reply(Event).

chan_event_reply(Event) when ?IS_HIDDEN_EVENT(Event) ->
	noevent;
chan_event_reply(Event) ->
	Event.

get_chan_info(FsmRef) ->
	gen_fsm:sync_send_event(FsmRef, chan_info, infinity).

%%% gen_fsm callbacks

init(Name) ->
	{ok, state_joining, #chan{name = Name}}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_Vsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

%%% States

state_joining({chantopic, _, Topic}, Chan) ->
	{next_state, state_topic, Chan#chan{topic = Topic}};
state_joining({names, _, Names}, #chan{users = Users} = Chan) ->
	{next_state, state_names, Chan#chan{users = Users ++ Names}}.

state_topic({chantopic, _, Author, Ts}, #chan{topic = Topic} = Chan) ->
	{next_state, state_names, Chan#chan{topic = {Topic, Author, Ts}}}.

state_names({names, _, Names}, #chan{users = Users} = Chan) ->
	{next_state, state_names, Chan#chan{users = Users ++ Names}};
state_names({endofnames, _, _}, Chan) ->
	{next_state, state_joined, Chan}.

state_names({endofnames, _, _}, _From, Chan) ->
	{reply, chan_info(Chan, as_event), state_joined, Chan}.

state_joined({parted, _}, Chan)  ->
	{stop, normal, Chan};
state_joined({kicked, _, _, _}, Chan) ->
	{stop, normal, Chan};
state_joined({kick, _, _, Nick, _}, Chan) ->
	{next_state, state_joined, remove_user(Nick, Chan)};
state_joined({part, _, ?USER(Nick), _}, Chan) ->
	{next_state, state_joined, remove_user(Nick, Chan)};
state_joined({join, _, ?USER(Nick)}, Chan) ->
	{next_state, state_joined, add_user(Nick, Chan)};
state_joined({mode, _, _, Mode, Nick}, Chan) ->
	{next_state, state_joined, change_mode(Nick, Mode, Chan)};
state_joined({mymode, _, _, Mode, MyNick}, Chan) ->
	{next_state, state_joined, change_mode(MyNick, Mode, Chan)};
state_joined({topic, _, ?USER(AuthorNick), Topic}, Chan) ->
	{next_state, state_joined, change_topic(Topic, AuthorNick, Chan)};
state_joined({mytopic, _, MyNick, Topic}, Chan) ->
	{next_state, state_joined, change_topic(Topic, MyNick, Chan)};
state_joined({irc_command, Fun, Lines}, Chan) ->
	[Fun(Line) || Line <- Lines],
	{next_state, state_joined, Chan};
state_joined(_, Chan) ->
	{next_state, state_joined, Chan}.

state_joined(chan_info, _From, Chan) ->
	{reply, chan_info(Chan, as_info), state_joined, Chan};
state_joined({nick, NewNick, ?USER(OldNick) = User}, _From, Chan) ->
	{reply, {nick, Chan#chan.name, NewNick, User}, state_joined, rename_user(OldNick, NewNick, Chan)};
state_joined({quit, ?USER(Nick) = User, Reason}, _From, Chan) ->
	{reply, {quit, Chan#chan.name, User, Reason}, state_joined, remove_user(Nick, Chan)}.

chan_info(#chan{name = Name, topic = Topic, users = Users}, as_event) ->
	{joined, Name, Topic, Users};
chan_info(#chan{name = Name, topic = Topic, users = Users}, as_info) ->
	{Name, Topic, Users}.

remove_user(Nick, #chan{users = Users} = Chan) ->
	Chan#chan{users = lists:keydelete(Nick, 2, Users)}.

add_user(Nick, #chan{users = Users} = Chan) ->
	Chan#chan{users = [{user, Nick, []} | Users]}.

rename_user(OldNick, NewNick, Chan) ->
	NewUsers = lists:map(fun ({T, N, F}) when N =:= OldNick -> {T, NewNick, F};
							 (User)                         -> User end, Chan#chan.users),
	Chan#chan{users = NewUsers}.

change_mode(Nick, [$- | Mode], Chan) ->
	remove_mode(Nick, Mode, Chan);
change_mode(Nick, [$+ | Mode], Chan) ->
	add_mode(Nick, Mode, Chan);
change_mode(Nick, Mode, Chan) ->
	add_mode(Nick, Mode, Chan).

%% Ops get added to tail, users -- to head

add_mode(Nick, [$o | Mode], Chan) ->
	{value, User, Others} = lists:keytake(Nick, 2, Chan#chan.users),
	add_mode(Nick, Mode, Chan#chan{users = Others ++ [setelement(1, User, op)]});
add_mode(Nick, [$v | Mode], Chan) ->
	NewUsers = lists:map(fun ({T, N, F}) when N =:= Nick -> {T, N, util:set_flag(voice, F)};
							 (User)                      -> User end, Chan#chan.users),
	add_mode(Nick, Mode, Chan#chan{users = NewUsers});
add_mode(Nick, [_ | Mode], Chan) ->
	add_mode(Nick, Mode, Chan);
add_mode(_, [], Chan) ->
	Chan.

remove_mode(Nick, [$o | Mode], Chan) ->
	{value, User, Others} = lists:keytake(Nick, 2, Chan#chan.users),
	remove_mode(Nick, Mode, Chan#chan{users = [setelement(1, User, user) | Others]});
remove_mode(Nick, [$v | Mode], Chan) ->
	NewUsers = lists:map(fun ({T, N, F}) when N =:= Nick -> {T, N, util:unset_flag(voice, F)};
							 (User)                -> User end, Chan#chan.users),
	remove_mode(Nick, Mode, Chan#chan{users = NewUsers});
remove_mode(Nick, [_ | Mode], Chan) ->
	remove_mode(Nick, Mode, Chan);
remove_mode(_, [], Chan) ->
	Chan.

%% TODO: handle `me' here somehow
change_topic(Topic, Nick, Chan) ->
	{M, S, _} = now(),
	Chan#chan{topic = {Topic, Nick, M*1000000 + S}}.
