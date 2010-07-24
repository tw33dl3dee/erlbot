%%%-------------------------------------------------------------------
%%% File    : erlbot_config.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_config).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-include_lib("eunit/include/eunit.hrl").

%%% API
-export([start_link/0, reload/0]).
-export([get_value/1, get_value/2, get_all_keys/0]).
-export([set_value/2, unset_value/1]).

%%% gen_server
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TAB_NAME, erlbot_config_tbl).
-define(DEFAULT_CONF_NAME, "erlbot.conf").

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_value(Name) -> get_value(Name, undefined).

get_value(Name, Default) -> 
	process_value(Name, ets:lookup(?TAB_NAME, Name), Default).

get_all_keys() ->
	ets:foldr(fun ({K, _}, Keys) -> [K | Keys] end, [], ?TAB_NAME).

set_value(Name, Value) ->
	gen_server:cast(?MODULE, {set_value, Name, Value}).

unset_value(Name) ->
	gen_server:cast(?MODULE, {unset_value, Name}).

reload() ->
	gen_server:call(?MODULE, reload).

%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init(_) ->
	Tab = ets:new(?TAB_NAME, [named_table]),
	case load_config() of
		{error, E} -> {'EXIT', E};
		_          -> {ok, Tab}
	end.

handle_call(reload, _From, State) ->
    {reply, load_config(), State};
handle_call(_Req, _From, State) ->
    {reply, unknown_call, State}.

handle_cast({set_value, Name, Value}, State) ->
	ets:insert(?TAB_NAME, {Name, Value}),
	{noreply, State};
handle_cast({unset_value, Name}, State) ->
	ets:delete(?TAB_NAME, Name),
	{noreply, State};
handle_cast(reload, State) ->
	load_config(),
	{noreply, State};
handle_cast(_Req, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

process_value(K, [{K, V}], _)  -> V;
process_value(K, [], required) -> throw({key_missing, K});
process_value(_, [], Default)  -> Default.

load_config() ->
	case load_config_terms(conf_files()) of
		{error, E} -> {error, E};
		Terms      -> log_config_load(set_new_config(Terms))
	end.

load_config_terms(ConfFiles) -> load_config_terms(dict:new(), ConfFiles).

load_config_terms(Terms, [ConfFile | ConfFiles]) ->
	case file:consult(ConfFile) of
		{error, {L, M, T}} -> 
			error_logger:error_report([config_error, {file, ConfFile}, {line, L}, {mod, M}, {term, T}]),
			{error, {ConfFile, {L, M, T}}};
		{error, E} ->
			error_logger:error_report([config_error, {file, ConfFile}, {error, E}]),
			{error, {ConfFile, E}};
		{ok, Terms2} ->
			error_logger:info_report([config_loaded, {file, ConfFile}]),
			load_config_terms(merge_config_terms(Terms, Terms2), ConfFiles)
	end;
load_config_terms(Terms, []) -> Terms.

merge_config_terms(CurTerms, NewTerms) ->
	D = dict:from_list(proplists:unfold(NewTerms)),
	dict:merge(fun (_K, _V1, V2) -> V2 end, CurTerms, D).

set_new_config(Terms) ->
	OldConfig = ets:tab2list(?TAB_NAME),
	NewConfig = dict:to_list(Terms),
	update_config(lists:keysort(1, OldConfig), 
				  lists:keysort(1, NewConfig), 
				  [], [], []).

update_config([{K1, V1} | T1] = L1, [{K2, V2} | T2] = L2, N, C, D) ->
	if K1 > K2  -> ets:insert(?TAB_NAME, {K2, V2}),
				   update_config(L1, T2, [{K2, V2} | N], C, D);
	   K1 < K2  -> ets:delete(?TAB_NAME, K1),
				   update_config(T1, L2, N, C, [{K1, V1} | D]);
	   V1 /= V2 -> ets:insert(?TAB_NAME, {K2, V2}),
				   update_config(T1, T2, N, [{K1, V1, V2} | C], D);
	   true     -> update_config(T1, T2, N, C, D)
	end;
update_config([], [{K2, V2} | T2], N, C, D) ->
	ets:insert(?TAB_NAME, {K2, V2}),
	update_config([], T2, [{K2, V2} | N], C, D);
update_config([{K1, V1} | T1], [], N, C, D) ->
	ets:delete(?TAB_NAME, K1),
	update_config(T1, [], N, C, [{K1, V1} | D]);
update_config([], [], N, C, D) ->
	{lists:reverse(N),
	 lists:reverse(C),
	 lists:reverse(D)}.

conf_files() ->
	case application:get_env(erlbot, config_files) of
		{ok, Val} -> Val;
		undefined -> [?DEFAULT_CONF_NAME]
	end.

log_config_load(Updates) ->
	case Updates of 
		{[], [], []} -> error_logger:info_report([config_unchanged]);
		{N, C, D}    -> error_logger:info_report([config_changed, {new, N}, {changed, C}, {deleted, D}]),
						error_logger:info_report([config | lists:keysort(1, ets:tab2list(?TAB_NAME))])
	end,
	Updates.

%%%-------------------------------------------------------------------
%%% Unit tests
%%%-------------------------------------------------------------------

stateless_test_() ->
	{foreach, local,
	 fun () -> ets:new(?TAB_NAME, [named_table]) end,
	 fun (Tab) -> ets:delete(Tab) end,

	 [?_assertEqual(update_config([{b, 1}, {c, 2}, {p, 3}, {q, 4}],
								  [{a, 0}, {b, 2}, {c, 2}, {r, 5}, {z, 6}],
								  [], [], []),
					{[{a, 0}, {r, 5}, {z, 6}],
					 [{b, 1, 2}],
					 [{p, 3}, {q, 4}]}
				   ),

	  ?_assertEqual(update_config([{b, 1}, {c, 2}, {p, 3}, {q, 4}],
								  [{a, 0}, {b, 2}, {c, 2}],
								  [], [], []),
					{[{a, 0}],
					 [{b, 1, 2}],
					 [{p, 3}, {q, 4}]}
				   ),

	  ?_assertEqual(begin D1 = dict:new(),
						  D2 = merge_config_terms(D1, [{c, 4}, a, {b, 1}, {c, 2}, {b, 3}, {d, 5}, {e, 6}]),
						  D3 = merge_config_terms(D2, [e, {b, 2}, {d, false}]),
						  D3
					end,
					dict:from_list([{a, true}, {b, 2}, {c, 2}, {d, false}, {e, true}])
				   ),

	  ?_assertEqual(begin D1 = dict:new(),
						  D2 = merge_config_terms(D1, [e, {b, 2}, {d, false}]),
						  D3 = merge_config_terms(D2, [{c, 4}, a, {b, 1}, {c, 2}, {b, 3}, {e, 6}]),
						  D3
					end,
					dict:from_list([{a, true}, {b, 3}, {c, 2}, {d, false}, {e, 6}])
				   )
	 ]}.

stateful_test_() ->
	{inorder,
	 {setup, local,
	  fun () -> ets:new(?TAB_NAME, [named_table]) end,
	  fun (Tab) -> ets:delete(Tab) end,

	 [?_assertEqual(set_new_config(dict:from_list([{b, 1}, {c, 2}, {p, 3}, {q, 4}])),
					{[{b, 1}, {c, 2}, {p, 3}, {q, 4}], 
					 [], 
					 []}
				   ),

	  ?_assertEqual(lists:keysort(1, ets:tab2list(?TAB_NAME)),
					[{b, 1}, {c, 2}, {p, 3}, {q, 4}]
				   ),
	  
	  ?_assertEqual(set_new_config(dict:from_list([{a, 0}, {b, 2}, {c, 2}, {r, 5}, {z, 6}])),
					{[{a, 0}, {r, 5}, {z, 6}],
					 [{b, 1, 2}],
					 [{p, 3}, {q, 4}]}
				   ),
	  
	  ?_assertEqual(lists:keysort(1, ets:tab2list(?TAB_NAME)),
					[{a, 0}, {b, 2}, {c, 2}, {r, 5}, {z, 6}]
				   )
	 ]}}.
