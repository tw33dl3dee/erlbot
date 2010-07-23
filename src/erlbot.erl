%%%-------------------------------------------------------------------
%%% File    : erlbot.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 18 Jul 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([add_behaviour/1, remove_behaviour/1]).
-export([reload_config/0, reload_code/0]).
-export([deploy/0]).

%%====================================================================
%%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->
	erlbot_sup:start_link(top),
	irc_conn:connect().

stop(_State) -> ok.

%%====================================================================
%%% API
%%====================================================================
add_behaviour(BhvMod) ->
	erlbot_sup:add_behaviour(BhvMod).

remove_behaviour(BhvMod) ->
	erlbot_sup:remove_behaviour(BhvMod).

reload_config() ->
	ok.

reload_code() ->
	CodeDir = code:lib_dir(erlbot, ebin),
	ModList = [begin true          = code:purge(Mod),
					 {module, Mod} = code:load_file(Mod),
					 Mod
			   end || {Mod, ModPath} <- code:all_loaded(), 
					  is_list(ModPath), lists:prefix(CodeDir, ModPath)],
	error_logger:info_report([{modules_reloaded, ModList}]),
	ModList.

deploy() ->
	ok = erlbot_db:init_db().
