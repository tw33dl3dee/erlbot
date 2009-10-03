-module(db_util).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

%% External interfaces
-export([create_sequence/0, create_sequence/1, init_sequence/2, sequence/1, sequence/2]).

-record(sequence, {table, idx}).

init_db() ->
	Node = node(),
	ok = case mnesia:create_schema([Node]) of
			 {error, {Node, {already_exists, Node}}} ->
				 ok;
			 E ->
				 E
		 end.

init_table(Name, TabDef) ->
	ok = init_db(),
	{atomic, ok} = case mnesia:create_table(Name, TabDef) of
					   {aborted, {already_exists, Name}} ->
						   {atomic, ok};
					   E ->
						   E
				   end.

create_sequence() ->
	create_sequence([node()]).
create_sequence(Nodes) ->
	init_table(sequence, [{type, set},
						  {disc_copies, Nodes},
						  {attributes, record_info(fields, sequence)}]).

init_sequence(Table, Idx) ->
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(#sequence{table = Table, idx = Idx}) end).

sequence(Table) ->
     sequence(Table, 1).
sequence(Table, Inc) ->
     mnesia:dirty_update_counter(sequence, Table, Inc).
