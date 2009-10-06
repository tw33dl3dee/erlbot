%%%-------------------------------------------------------------------
%%% File    : bhv_appeal.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 2 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_appeal).

-behaviour(irc_behaviour).
-export([handle_event/3]).

-include("utf8.hrl").
-include("irc.hrl").

handle_event(_Type, _Event, _Conn) ->
	not_handled.

%% TODO: own nick
