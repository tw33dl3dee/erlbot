%%%-------------------------------------------------------------------
%%% File    : bhv_greet.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_greet).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

%% List of channels already presented ourselves on.
init(_) -> [].

help(_) -> none.

handle_event(chanevent, {joined, Chan, _, _}, _, Data) ->
	case lists:member(Chan, Data) of
		true -> not_handled;
		false -> greet(Chan),
				 {ok, [Chan | Data]}
	end;
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

-define(GREETINGS, ["shaking...",
					"liquefying bytes... ",
					"homogenizing goo... ",
					"testing ozone... ",
					"processing... ",
					"spinning violently around the y-axis... ",
					"iodizing... ",
					"stretching images... ",
					"reconstituting sounds... ",
					"faithfully re-imagining... ",
					"scraping funds... ",
					"applying innovation... ",
					"constructing emotional depth... ",
					"debating games as art... ",
					"placating publishers.., ",
					"meticulously diagramming fun... ",
					"filtering moral... ",
					"testing for perfection... ",
					"revolving independence... ",
					"tokenizing innovation... ",
					"self affirming... ",
					"dissolving relationships... ",
					"deterministically simulating the future... ",
					"exceeding cpu quota... ",
					"swapping time and space... ",
					"embiggening prototypes... ",
					"sandbagging expectations... ",
					"challenging everything... ",
					"distilling beauty... ",
					"blitting powers of two... ",
					"manufacturing social responsibility... ",
					"bending the spoon... ",
					"constructing non-linear narrative..."]).

%% Min and max number of greeting messages bot displays on join
-define(MIN_GREETS, 3).
-define(MAX_GREETS, 6).

greet(Chan) ->
	NumGreets = choice:uniform(?MIN_GREETS, ?MAX_GREETS),
	Greets = [choice:make(?GREETINGS) || _I <- lists:seq(1, NumGreets)],
	ok = irc_conn:bulk_action(Chan, nohist, Greets).
