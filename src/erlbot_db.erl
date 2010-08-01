%%%-------------------------------------------------------------------
%%% File    : erlbot_db.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created :  1 Aug 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_db).

-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% API
-export([start_link/0]).
-export([create_sequence/0, create_sequence/1, init_sequence/2, sequence/1, sequence/2, init_db/0, init_table/2]).

-include("couchbeam.hrl").

-record(sequence, {table, idx}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
	Params = erlbot_config:get_value(couchdb_params, required),
	DbName = erlbot_config:get_value(couchdb_dbname, required),
	DbParam = #couchdb_params{name = erlbot,
							  host = proplists:get_value(host, Params),
							  username = proplists:get_value(username, Params),
							  password = proplists:get_value(password, Params)},
	case couchbeam_server:start_connection_link(DbParam) of
		Pid when is_pid(Pid) -> 
			couchbeam_server:open_db(Pid, {?MODULE, DbName}),
			{ok, Pid};
		Res -> Res
	end.

init_db() ->
	Node = node(),
	ok = case mnesia:create_schema([Node]) of
			 {error, {Node, {already_exists, Node}}} -> ok;
			 E                                       -> E
		 end,
	ok = mnesia:start().

init_table(Name, TabDef) ->
	{atomic, ok} = case mnesia:create_table(Name, TabDef) of
					   {aborted, {already_exists, Name}} -> {atomic, ok};
					   E                                 -> E
				   end,
	ok = mnesia:wait_for_tables([Name], infinity).

create_sequence() -> create_sequence([node()]).

create_sequence(Nodes) ->
	init_table(sequence, [{type, set}, {disc_copies, Nodes},
						  {attributes, record_info(fields, sequence)}]).

init_sequence(Table, Idx) ->
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(#sequence{table = Table, idx = Idx}) end).

sequence(Table) -> sequence(Table, 1).

sequence(Table, Inc) -> mnesia:dirty_update_counter(sequence, Table, Inc).

%%====================================================================
%% Internal functions
%%====================================================================
