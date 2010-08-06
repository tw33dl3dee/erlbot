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
-export([query_view/2, foldl_view/4, create_design_docs/0]).

-include("couchbeam.hrl").

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
		{error, Err} -> error_logger:error_report([query_view, {view_name, ViewName}, 
												   {params, Params}, {error, Err}]),
						{error, Err};
		Result -> Result
	end.

%% Optimum lies somewhere in [1000, 4000]
-define(MAX_FETCH_ENTRIES, 2000).

foldl_view(Fun, Acc, ViewName, Params) ->
	Params1 = proplists:delete(limit, Params),
	Params2 = proplists:delete(startkey, Params1),
	case query_view(ViewName, [{limit, ?MAX_FETCH_ENTRIES} | Params1]) of
		{_, _, _, []}  -> Acc;
		{_, _, _, Res} -> foldl_view_cont(Fun, Acc, Res, ViewName, Params2)
	end.

foldl_view_cont(Fun, Acc, [LastRow], ViewName, Params) ->
	NextParams = case LastRow of 
					 {undefined, Key, _}    -> [{startkey, Key} | Params];
					 {undefined, Key, _, _} -> [{startkey, Key} | Params];
					 {DocId, Key, _}        -> [{startkey, Key}, {startkey_docid, DocId} | Params];
					 {DocId, Key, _, _}     -> [{startkey, Key}, {startkey_docid, DocId} | Params]
				 end,
	case query_view(ViewName, [{limit, ?MAX_FETCH_ENTRIES} | NextParams]) of
		{_, _, _, [LastRow]} -> Fun(LastRow, Acc);
		{_, _, _, NextRes}   -> foldl_view_cont(Fun, Acc, NextRes, ViewName, Params)
	end;
foldl_view_cont(Fun, Acc, [Row | Rest], ViewName, Params) ->
	foldl_view_cont(Fun, Fun(Row, Acc), Rest, ViewName, Params).

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

%%====================================================================
%% Internal functions
%%====================================================================

save_design_doc(FilePath) ->
	{ok, Json} = file:read_file(FilePath),
	case couchbeam_db:save_doc(?MODULE, couchbeam:json_decode(Json)) of
		{_}      -> ok;
		conflict -> ok;  %% doc already saved
		Err      -> error_logger:error_report([save_design_doc, {doc, Json}, {error, Err}])
	end.
