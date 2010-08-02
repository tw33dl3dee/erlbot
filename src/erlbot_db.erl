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
-export([query_view/2, create_design_docs/0]).

-include("couchbeam.hrl").

-record(sequence, {table, idx}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
	Params = erlbot_config:get_value(couchdb_params, required),
	DbName = erlbot_config:get_value(couchdb_dbname, required),
	DbParam = #couchdb_params{name = erlbot,
							  host = proplists:get_value(host, Params, "127.0.0.1"),
							  username = proplists:get_value(username, Params, nil),
							  password = proplists:get_value(password, Params, nil)},
	case couchbeam_server:start_connection_link(DbParam) of
		Pid when is_pid(Pid) -> 
			case couchbeam_server:open_or_create_db(Pid, {?MODULE, DbName}) of
				DbPid when is_pid(DbPid) ->
					ok = create_design_docs(),
					{ok, Pid};
				Err1 -> Err1
			end;
		Res -> Res
	end.

query_view(ViewName, Params) ->
	View = couchbeam_db:query_view(?MODULE, ViewName, Params),
	Data = couchbeam_view:parse_view(View),
	couchbeam_view:close_view(View),
	case Data of 
		{error, Err} -> error_logger:report_error([query_view, {view_name, ViewName}, 
												   {params, Params}, {error, Err}]),
						{error, Err};
		Result -> Result
	end.

-define(DESIGN_DOC_DIR, "priv/couchdb/design").

%% Create all design docs listed in `DESIGN_DOC_DIR'
create_design_docs() ->
	filelib:fold_files(?DESIGN_DOC_DIR, "\\.json$", %"
					   false,
					   fun (File, Acc) ->
							   case save_design_doc(File) of
								   ok  -> Acc;
								   Err -> Err
							   end
					   end, 
					   ok).

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

save_design_doc(FilePath) ->
	{ok, Json} = file:read_file(FilePath),
	case couchbeam_db:save_doc(?MODULE, couchbeam:json_decode(Json)) of
		{_}      -> ok;
		conflict -> ok;  %% doc already saved
		Err      -> io:format("~p~n", [Err])
	end.
