%%%-------------------------------------------------------------------
%%% File    : bhv_btce.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 29 Nov 2013 by Ivan Korotkov <tw33dl3.dee@gmail.com>
%%%-------------------------------------------------------------------
-module(bhv_btce).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4, config_change/2, diff_rates/3]).

-include("utf8.hrl").
-include("irc.hrl").

%% Period between subsequent rate checks (msec).
-define(INITIAL_CHECK_PERIOD, 5000).
-define(CHECK_PERIOD, 300000).

init({Chans, Rates}) -> {false, [], Chans, Rates}.

config_change({Chans, Rates}, {InitDone, LastRates, _, _}) ->
    {ok, {InitDone, LastRates, Chans, Rates}}.

%% TODO(twee)
help(_) -> none.

handle_event(_, _, _, {false, _, Chans, Rates}) ->
    {delayed_event, ?INITIAL_CHECK_PERIOD, customevent, check_rates,
     {true, [], Chans, Rates}};
handle_event(customevent, check_rates, _, {_, LastRates, Chans, Rates} = D) ->
    NewRates = get_rates(Rates),
    case diff_rates(LastRates, NewRates, Rates) of
        [] -> {delayed_event, ?CHECK_PERIOD, customevent, check_rates, D};
        ChangedRates ->
            [announce_change(Chan, Change)
             || Chan <- Chans, Change <- ChangedRates],
            {delayed_event, ?CHECK_PERIOD, customevent, check_rates,
             {true, NewRates, Chans, Rates}}
    end;
handle_event(cmdevent, {chancmd, Chan, _, ["btce", Rate]}, _, _) ->
    ok = bhv_common:pipe_script(Chan, "btc-e.py", cmd_args(Rate));
handle_event(_Type, _Event, _IrcState, _Data) ->
    not_handled.

get_rates([{Rate, RateCode, _} | T]) -> 
	% BUG: use code:priv_dir
    {success, [Price]} =
        erlbot_util:execv("btc-e.py", cmd_args(RateCode), "priv/bin"),
    get_rates(T) ++ [{Rate, list_to_number(Price)}];
get_rates([]) -> [].

cmd_args(Rate) -> ["-r", Rate, "-f", "buy"].

list_to_number(Str) -> 
    try float(list_to_integer(Str)) 
    catch error:_ -> list_to_float(Str) 
    end.

-define(RATE(Rate), {Rate, _, _}).

diff_rates([{Rate, Price1} | T1], [{Rate, Price2} | T2],
           [{Rate, _, MaxDiff} | T3]) when abs(Price1 - Price2) > MaxDiff ->
    ok = error_logger:info_report([irc_event, {change, [Rate, abs(Price1 - Price2), MaxDiff]}]),
    diff_rates(T1, T2, T3) ++ [{Rate, Price1, Price2}];
%% rate unchanged
diff_rates([{Rate, _} | T1], [{Rate, _} | T2], [?RATE(Rate) | T3]) ->
    diff_rates(T1, T2, T3);
%% rate vanished (fetch error), simply skip it
diff_rates([{Rate, _} | T1], New, [?RATE(Rate) | T3]) ->
    diff_rates(T1, New, T3);
%% new rate (announce initially)
diff_rates(Old, [{Rate, Price} | T2], [?RATE(Rate) | T3]) ->
    diff_rates(Old, T2, T3) ++ [{Rate, Price}];
%% no longer watching this rate, skip it
diff_rates([_ | T1], New, Rates) ->
    diff_rates(T1, New, Rates);
diff_rates([], [], []) -> [].

announce_change(Chan, {Rate, InitialPrice}) ->
    ok = irc_conn:chanmsg(Chan, hist,
                          % bold
                          [2, Rate, ": ", price_to_list(InitialPrice), 15]);
announce_change(Chan, {Rate, Price1, Price2}) when Price1 > Price2 ->
    ok = irc_conn:chanmsg(Chan, hist,
                          % bold, green
                          [2, Rate, ": ", price_to_list(Price1),
                           " -> ", 3, "03", price_to_list(Price2), 15]);
announce_change(Chan, {Rate, Price1, Price2}) ->
    ok = irc_conn:chanmsg(Chan, hist,
                          % bold, red
                          [2, Rate, ": ", price_to_list(Price1),
                           " -> ", 3, "04", price_to_list(Price2), 15]).

price_to_list(Price) -> io_lib:format("~.2f", [Price]).
