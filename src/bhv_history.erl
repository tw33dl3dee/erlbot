%%%-------------------------------------------------------------------
%%% File    : bhv_history.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_history).

-behaviour(irc_behaviour).
-export([init/1, handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").
-include_lib("stdlib/include/qlc.hrl").

-record(user, {uid    :: integer(),
			   ident  :: list()}). 
-record(chan, {cid   :: integer(),
			   name  :: list()}). 
-record(histent, {timestamp  :: {{integer(), integer(), integer()}, 
								 {integer(), integer(), integer()}, 
								 integer()},
				  uid        :: integer(),
				  cid        :: integer(),
				  event      :: tuple()}).

init(_) -> 
	db_util:create_sequence(),
	{atomic, ok} = db_util:init_table(user, [{disc_copies, [node()]},
											 {index, [#user.ident]},
											 {attributes, record_info(fields, user)}]),
	{atomic, ok} = db_util:init_table(chan, [{disc_copies, [node()]},
											 {index, [#chan.name]},
											 {attributes, record_info(fields, chan)}]),
	{atomic, ok} = db_util:init_table(histent, [{disc_copies, [node()]},
												{type, ordered_set},
												{attributes, record_info(fields, histent)}]).

handle_event(genevent, {chanmsg, Chan, ?USER2(Nick, Ident), Msg}, _Irc) ->
	save_histent(Chan, Ident, {chanmsg, Nick, Msg});
handle_event(genevent, {action, Chan, ?USER2(Nick, Ident), Msg}, _Irc) ->
	save_histent(Chan, Ident, {action, Nick, Msg});
handle_event(selfevent, {chanmsg, Chan, Msg}, Irc) ->
	save_histent(Chan, me, {chanmsg, me(Irc), Msg});
handle_event(selfevent, {action, Chan, Msg}, Irc) ->
	save_histent(Chan, me, {action, me(Irc), Msg});
handle_event(chanevent, {topic, Chan, ?USER2(Nick, Ident), Topic}, _Irc) ->
	save_histent(Chan, Ident, {topic, Nick, Topic});
handle_event(chanevent, {mytopic, Chan, _, Topic}, Irc) ->
	save_histent(Chan, me, {topic, me(Irc), Topic});
handle_event(chanevent, {join, Chan, ?USER2(Nick, Ident)}, _Irc) ->
	save_histent(Chan, Ident, {join, Nick});
handle_event(chanevent, {joined, Chan, Topic, _}, Irc) ->
	save_histent(Chan, me, {joined, me(Irc), Topic});
handle_event(chanevent, {part, Chan, ?USER2(Nick, Ident), Reason}, _Irc) ->
	save_histent(Chan, Ident, {part, Nick, Reason});
handle_event(chanevent, {parted, Chan}, Irc) ->
	save_histent(Chan, me, {part, me(Irc), []});  %% fixme(Irc)
handle_event(chanevent, {quit, Chan, ?USER2(Nick, Ident), Reason}, _Irc) ->
	save_histent(Chan, Ident, {quit, Nick, Reason});
handle_event(chanevent, {kick, Chan, ?USER2(Nick1, Ident), Nick2, Reason}, _Irc) ->
	save_histent(Chan, Ident, {kick, Nick1, Nick2, Reason});
handle_event(chanevent, {kicked, Chan, ?USER2(Nick, Ident), Reason}, Irc) ->
	save_histent(Chan, Ident, {kick, Nick, me(Irc), Reason});
handle_event(chanevent, {mode, Chan, ?USER2(Nick1, Ident), Mode, Nick2}, _Irc) ->
	save_histent(Chan, Ident, {mode, Nick1, Mode, Nick2});
handle_event(chanevent, {mymode, Chan, ?USER2(Nick, Ident), Mode, _}, Irc) ->
	save_histent(Chan, Ident, {mode, Nick, Mode, me(Irc)});
handle_event(_Type, _Event, _Irc) ->
	not_handled.

save_histent(Chan, Ident, Event) ->
	io:format("^^^ saving histent (~p, ~p, ~p)~n", [Chan, Ident, Event]),
	ok = mnesia:async_dirty(fun () ->
									mnesia:write(#histent{timestamp = timestamp(),
														  uid       = userid(Ident),
														  cid       = chanid(Chan),
														  event     = Event})
							end).

me(#irc{nick = Nick}) -> Nick.

timestamp() ->
	{{Y, M, D}, {HH, MM, SS}} = erlang:universaltime(),
	{_, _, U} = erlang:now(),
	{{-Y, -M, -D}, {-HH, -MM, -SS}, -U}.

userid(me) -> 0;
userid(Ident) ->
	Q = qlc:q([U#user.uid || U <- mnesia:table(user), U#user.ident =:= Ident]),
	case qlc:e(Q) of
		[Id] -> Id;
		[]   -> 
			Id = db_util:sequence(user),
			mnesia:write(#user{uid = Id, ident = Ident}),
			Id
	end.

chanid(Channel) ->
	Q = qlc:q([Ch#chan.cid || Ch <- mnesia:table(chan), Ch#chan.name =:= Channel]),
	case qlc:e(Q) of
		[Id] -> Id;
		[]   -> 
			Id = db_util:sequence(channel),
			mnesia:write(#chan{cid = Id, name = Channel}),
			Id
	end.
