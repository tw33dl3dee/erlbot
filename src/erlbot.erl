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
-export([start/2, stop/1, config_change/3]).

%% API
-export([start/0, deploy/0]).
-export([add_behaviour/1, remove_behaviour/1]).
-export([reload_config/0, reload_code/0, reload/0]).

%%====================================================================
%%% Application callbacks
%%====================================================================
start(_Type, _StartArgs) ->
	erlbot_sup:start_link(top).

stop(_State) -> ok.

config_change(_Changed, _New, _Removed) ->
	ok = reload_config().

%%====================================================================
%%% API
%%====================================================================
start() -> application:start(?MODULE).

add_behaviour(BhvMod) ->
	erlbot_sup:add_behaviour(BhvMod).

remove_behaviour(BhvMod) ->
	erlbot_sup:remove_behaviour(BhvMod).

reload_config() ->
	case erlbot_config:reload() of
		{[], [], []} -> ok;
		{C, N, R}    -> erlbot_sup:config_change(C, N, R)
	end.

reload_code() ->
	ModList = [begin code:purge(M), code:load_file(M) 
			   end || {M, F} <- code:all_loaded(), is_list(F), is_changed(M)],
	error_logger:info_report([modules_reloaded | ModList]),
	{ok, ModList}.

reload() ->
	{reload_code(),
	 reload_config()}.

deploy() ->
	ok = erlbot_db:create_design_docs().

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

%%% Taken from mochiweb `reloader' module

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try module_vsn(M:module_info(attributes)) =/= module_vsn(code:get_object_code(M))
    catch _:_ -> false
    end.

module_vsn({M, Beam, _Fn}) ->
    {ok, {M, Vsn}} = beam_lib:version(Beam),
    Vsn;
module_vsn(Attrs) when is_list(Attrs) ->
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.
