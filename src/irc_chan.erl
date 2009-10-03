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

%%% Public interface

start(Name) ->
	gen_fsm:start(?MODULE, Name, []).

start_link(Name) ->
	gen_fsm:start_link(?MODULE, Name, []).

chan_event(FsmRef, Event) when element(1, Event) =:= endofnames ->
	gen_fsm:sync_send_event(FsmRef, Event, infinity);
chan_event(FsmRef, Event) ->
	gen_fsm:send_event(FsmRef, Event),
	event(Event).

event({parted, _} = E) ->
	E;
event({kicked, _, _, _} = E) ->
	E;
event(_) ->
	noevent.	

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
	{reply, chan_info(Chan), state_joined, Chan}.

state_joined({parted, _}, Chan)  ->
	{stop, normal, Chan};
state_joined({kicked, _, _, _}, Chan) ->
	{stop, normal, Chan};
state_joined(_, Chan) ->
	{next_state, state_joined, Chan}.

state_joined(chan_info, _From, Chan) ->
	{reply, chan_info(Chan), joined, Chan}.

chan_info(#chan{name = Name, topic = Topic, users = Users}) ->
	{joined, Name, Topic, Users}.
