%%%-------------------------------------------------------------------
%%% File    : erlbot_config.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_config).

%% API
-export([init/0, get_value/1, get_value/2, set_value/2, unset_value/1, get_all_keys/0]).

-record(config, {key    :: atom(),
				 value  :: term()}).

init() ->
	ok = erlbot_db:init_table(config, [{disc_copies, [node()]}, {attributes, record_info(fields, config)}]).

get_value(Name, Default) -> 
	case mnesia:dirty_read(config, Name) of
		[]                       -> Default;
		[#config{value = Value}] -> Value
	end.

get_value(Name) -> get_value(Name, undefined).

set_value(Name, Value) ->
	mnesia:dirty_write(#config{key = Name, value = Value}).

unset_value(Name) ->
	mnesia:dirty_delete(config, Name).

get_all_keys() ->
	mnesia:dirty_all_keys(config).
