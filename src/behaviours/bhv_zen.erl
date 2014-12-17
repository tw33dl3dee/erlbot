%%%-------------------------------------------------------------------
%%% File    : bhv_btce.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Dec 2014 by Ivan Korotkov <tw33dl3.dee@gmail.com>
%%%-------------------------------------------------------------------
-module(bhv_zen).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4, config_change/2]).

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
    {delayed_event, ?INITIAL_CHECK_PERIOD, customevent, check_rates, {true, [], Chans, Rates}};
handle_event(customevent, check_rates, _, {_, LastRates, Chans, Rates} = D) ->
    NewRates = get_rates(LastRates),
    case diff_rates(LastRates, NewRates, Rates) of
        [] ->
            {delayed_event, ?CHECK_PERIOD, customevent, check_rates, D};
        ChangedRates ->
            [announce_change(Chan, Change) || Chan <- Chans, Change <- ChangedRates],
            {delayed_event, ?CHECK_PERIOD, customevent, check_rates,
             {true, update_rates(LastRates, ChangedRates, Rates), Chans, Rates}}
    end;
handle_event(_Type, _Event, _IrcState, _Data) ->
    not_handled.

get_rates(LastRates) ->
    % BUG: use code:priv_dir
    case erlbot_util:execv("zenrus.py", [], "priv/bin") of
        {success, [USD, EUR]} ->
            [{"USD", list_to_number(USD)}, {"EUR", list_to_number(EUR)}];
        _ ->
            LastRates
    end.

list_to_number(Str) ->
    try float(list_to_integer(Str))
    catch error:_ -> list_to_float(Str)
    end.

-define(RATE(Rate), {Rate, _, _}).

diff_rates([{Rate, Price1} | T1], [{Rate, Price2} | T2], [{Rate, _, MaxDiff} | T3])
  when abs(Price1 - Price2) > MaxDiff ->
    [{Rate, Price1, Price2} | diff_rates(T1, T2, T3)];
%% rate unchanged
diff_rates([{Rate, _} | T1], [{Rate, _} | T2], [?RATE(Rate) | T3]) ->
    diff_rates(T1, T2, T3);
%% rate vanished (fetch error), simply skip it
diff_rates([{Rate, _} | T1], New, [?RATE(Rate) | T3]) ->
    diff_rates(T1, New, T3);
%% new rate (announce initially)
diff_rates(Old, [{Rate, Price} | T2], [?RATE(Rate) | T3]) ->
    [{Rate, Price} | diff_rates(Old, T2, T3)];
%% no longer watching this rate, skip it
diff_rates([_ | T1], New, Rates) ->
    diff_rates(T1, New, Rates);
diff_rates([], [], []) -> [].

update_rates(LastRates, [{Rate, InitialPrice} | T2], [{Rate, _, _} | T3]) ->
    [{Rate, InitialPrice} | update_rates(LastRates, T2, T3)];
update_rates([{Rate, _} | T1], [{Rate, _, NewPrice} | T2], [{Rate, _, _} | T3]) ->
    [{Rate, NewPrice} | update_rates(T1, T2, T3)];
update_rates([{Rate, _} = Unchanged | T1], ChangedRates, [{Rate, _, _} | T3]) ->
    [Unchanged | update_rates(T1, ChangedRates, T3)];
update_rates([_ | T1], ChangedRates, Rates) ->
    update_rates(T1, ChangedRates, Rates);
update_rates([], [], []) -> [].

announce_change(Chan, {Rate, InitialPrice}) ->
    ok = irc_conn:chanmsg(Chan, hist, [2, Rate, ": ", price_to_list(InitialPrice), 15]);  % bold
announce_change(Chan, {Rate, Price1, Price2}) when Price1 > Price2 ->
    ok = irc_conn:chanmsg(Chan, hist, [2, Rate, ": ", price_to_list(Price1), " -> ",
                                       3, "03", price_to_list(Price2), 15]);  % bold, green
announce_change(Chan, {Rate, Price1, Price2}) ->
    ok = irc_conn:chanmsg(Chan, hist, [2, Rate, ": ", price_to_list(Price1),
                                       " -> ", 3, "04", price_to_list(Price2), 15]).  % bold, red

price_to_list(Price) -> io_lib:format("~.2f", [Price]).
