%%%-------------------------------------------------------------------
%%% File    : flt_skel.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  8 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(flt_skel).

-behaviour(erlbot_filter).
-export([init/1, filter_command/3]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

filter_command(_Command, _IrcState, _Data) ->
	not_handled.
