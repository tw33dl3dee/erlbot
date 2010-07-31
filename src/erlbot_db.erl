%%%-------------------------------------------------------------------
%%% File    : erlbot_db.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_db).

-author("Ivan Korotkov <twee@tweedle-dee.org>").

-include("couchbeam.hrl").

%% External interfaces
-export([create_sequence/0, create_sequence/1, init_sequence/2, sequence/1, sequence/2, init_db/0, init_table/2]).
-export([couchdb/0]).

-record(sequence, {table, idx}).

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

couchdb() ->
	couchbeam:start(),
	C = couchbeam_server:start_connection_link(#couchdb_params{host = "twee.cc"}),
	Db = couchbeam_db:open(C, "erlbot"),
	{C, Db}.
