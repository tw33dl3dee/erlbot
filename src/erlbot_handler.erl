%%%-------------------------------------------------------------------
%%% File    : erlbot_handler.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 26 Sep 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(erlbot_handler).
-author("Ivan Korotkov <twee@tweedle-dee.org>").

-behaviour(gen_event).

%% External exports
-export([add_handler/2]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").

%% -record(userstat, {ident, line_count = 1, sym_count = undefined}).

-define(OP_BOT_NICK, "dumbot").

-record(state, {channels = [], 
				nick = [], 
				irc_ref = 0, 
				was_kicked = false, 
				kicked_by = undefined, 
				history = [], 
				topic = []}).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
add_handler(EventMgr, IrcRef) ->
	event_sup:start_link(EventMgr, undef, ?MODULE, IrcRef).

%% deploy() ->
%% 	{atomic, ok} = erlbot_db:init_table(userstat, [{disc_copies, [node()]}, {type, set}, {index, []}, {attributes, record_info(fields, userstat)}]).

%%%-------------------------------------------------------------------
%%% Callback functions from gen_event
%%%-------------------------------------------------------------------

init(IrcRef) ->
	{ok, #state{}}.

handle_call(_Request, State) ->
	{ok, nosuchcall, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_Vsn, State, _Extra) ->
	{ok, State}.

handle_event({Type, Event}, State) ->
	io:format("*** ~p: ~p~n", [Type, Event]),
	gen_event:notify(self(), noevent),
	%{ok, handle_anyevent(E, update_history(State))}.
	{ok, State};
handle_event(Event, State) ->
	io:format("*** ~p~n", [Event]),
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% -define(IRC(S), proc:where(S#state.irc_ref)).

%% cmd(Cmd, State) ->
%% 	irc_conn:irc_command(?IRC(State), Cmd).

%% %% handle_event_update_stat(Event, State) ->
%% %% 	erlbot_stat:update_stat(Event),
%% %% 	handle_anyevent(Event, update_history(Event, State)).

%% handle_anyevent({chanevent, Event}, State) ->
%% 	handle_chanevent(Event, State);
%% handle_anyevent({genevent, Event}, State) ->
%% 	handle_genevent(Event, State)).

%% %% update_stat(_, me, [User, Msg]) ->
%% %% 	update_stat(User, Msg);
%% %% update_stat(_, chanmsg, [User, Msg]) ->
%% %% 	update_stat(User, Msg);
%% %% update_stat(_, _, _) ->
%% %% 	ok.


%% %% update_history(Event, State) ->
%% %% 	io:format("Histent: ~p~n", [histent_to_list({Event})]),
%% %% 	Time = erlang:localtime(),
%% %% 	State#state{history = lists:sublist([{Time, Event} | State#state.history], ?MAX_HIST_LEN)}.

%% become_op(State) ->
%% 	cmd({privmsg, [?OP_BOT_NICK, ?GIVEOP_MAGIC_WORD]}, State),
%% 	State.

%% check_empty_topic(Chan, {[], _, _}, State) ->
%% 	do_lynch(Chan, topic, State).
%% check_empty_topic(_, _, State) ->
%% 	State.

%% handle_chanevent({joined, Chan, Topic, Users}, #state{was_kicked = true, kicked_by = Kicker} = State) ->
%% 	cmd({chanmsg, [suicide_greeting(State, Kicker)]}, State),
%% 	become_op(State),
%% 	State#state{was_kicked = false};
%% handle_chanevent({joined, Chan, Topic, Users}}, State) ->
%% 	become_op(State),
%% 	check_empty_topic(Chan, Topic, State),
%% handle_chanevent({kicked, Chan, User, _Reason}, State) ->
%% 	cmd({join, Chan}, State),
%% 	State#state{was_kicked = true, kicked_by = User};
%% handle_chanevent(_, State) ->
%% 	State.

%% handle_event2({topic, Chan, User, Topic}, State) ->
%% 	comment_topic(Chan, User, Topic, State);
%% handle_event2({join, User, Chan}, State) ->
%% 	comment_join(Chan, User, State);
%% handle_event2({privmsg, User, Msg}, State) ->
%% 	{ok, Cmd} = regexp:split(Msg, " "),
%% 	parse_privcmd(User, Cmd, State),
%% handle_event2({chanmsg, Chan, User, [$! | Rest]}, State) ->
%% 	{ok, Cmd} = regexp:split(Rest, " "),
%% 	parse_cmd(Chan, User, Cmd, State);
%% handle_event2({part, User, Chan, _Reason}, State) ->
%% 	comment_exit(User, Chan, State);
%% handle_event2({quit, User, _Reason}, State) ->
%% 	comment_exit(User, State);
%% handle_event2({chanmsg, Chan, User, Msg}, State) ->
%% 	AppealRE = lists:flatten(io_lib:format("^[ \t]*~s[:, ]\\(.*\\)$", [State#state.nick])), %"
%% 	case gregexp:groups(Msg, AppealRE) of
%% 		{match, [Rest]} ->
%% 			handle_appeal(Chan, User, Rest, State);
%% 		nomatch ->
%% 			handle_genmsg(Chan, User, Msg, State)
%% 	end;
%% handle_event2({action, Chan, User, Msg}, State) ->
%% 	handle_genmsg(Chan, User, Msg, State);
%% handle_event2(_Event, State) ->
%% 	do_blurp(State).

%% handle_appeal(Chan, User, Msg, State) ->
%% 	timer:sleep(1000),
%% 	case regexp:match(Msg, "(хуй|ХУЙ|заткни|ЗАТКНИ)") of
%% 		{match, _, _} ->
%% 			cmd({chanmsg, [User#user.nick ++ ": хамишь, сцуко."]}, State);
%% 		nomatch ->
%% 			case regexp:match(Msg, "(превед|ПРЕВЕД)") of
%% 				{match, _, _} ->
%% 					cmd({chanmsg, ["\\O/ Превед, " ++ User#user.nick ++ "!!!"]}, State);
%% 				nomatch ->
%% 					case regexp:match(Msg, "(уебись|УЕБИСЬ|сос(н)?и|СОС(Н)?И)") of 
%% 						{match, _, _} ->
%% 							do_suicide(State, User#user.nick);
%% 						nomatch ->
%% 							case regexp:match(Msg, "(няшка|НЯШКА|кавай|КАВАЙ)") of 
%% 								{match, _, _} ->
%% 									do_neko(State);
%% 								_ ->
%% 									cmd({chanmsg, ["Ня!"]}, State)
%% 							end
%% 					end
%% 			end
%% 	end.

%% handle_general(State, User, Msg) ->
%% 	case correct_misspell(Msg) of
%% 		no_misspell ->
%% 			do_blurp(State),
%% 			ok;
%% 		Corrected ->
%% 			do_correct(State, User, Corrected),
%% 			corrected
%% 	end.

%% %% Turn misspell correction off for now
%% -define(MISSPELL_THRESHOLD, 2).		%% How much % of latin characters we need in string to consider it possible mis-layouting.

%% correct_misspell(Msg) ->
%% 	LatinRatio = case yadbot_util:count_letters(Msg) of
%% 					 0 -> 0;
%% 					 X -> 1.0*yadbot_util:count_latin(Msg)/X
%% 				 end,
%% 	io:format("Checking spelling: ~p latins~n", [LatinRatio]),
%% 	correct_misspell(Msg, LatinRatio).

%% correct_misspell(_, Latins) when Latins < ?MISSPELL_THRESHOLD ->
%% 	no_misspell;
%% correct_misspell(Msg, _) ->
%% 	XlitCmd = lists:flatten(io_lib:format("~s '~s'", ["xlit2.pl", sanitize(Msg, $')])), % stop emacs from sucking -> '
%% 	case regexp:split(yadbot_util:system(?SCRIPT_DIR, XlitCmd), "\n") of
%% 		{ok, []} ->
%% 			no_misspell;
%% 		{ok, [Corrected]} ->
%% 			Corrected
%% 	end.

%% do_correct(State, _User, Corrected) ->
%% 	cmd({chanmsg, [Corrected]}, State).

%% do_blurp(State) ->
%% 	do_blurp(State, random_p:uniform()).

%% do_blurp(State, X) when X < 0.025 ->
%% 	timer:sleep(1000),
%% 	Words = ["Хамите", "Хо-хо!", "Знаменито", "Мрак", "Жуть", "Не учите меня жить", 
%% 			 "Как ребёнка", "Кр-р-расота!", "Толстый и красивый", "Поедем на извозчике",
%% 			 "Поедем на таксо", "У вас вся спина белая", "Подумаешь!", "Ого!"],
%% 	cmd({chanmsg, [lists:nth(random_p:uniform(length(Words)), Words)]}, State),
%% 	ok;
%% do_blurp(_, _) ->
%% 	ok.

%% parse_cmd(State, _, ["mooon"]) ->
%% 	cmd({chanmsg, [yadbot_util:system("pom")]}, State),
%% 	ok;
%% parse_cmd(State, User, [[$# | Rest]]) ->
%% 	case catch list_to_integer(Rest) of
%% 		{'EXIT', _} ->
%% 			unknown_cmd;
%% 		Num when Num > 0 ->
%% 			io:format("Bash query: ~p~n", [Num]),
%% 			do_bash(State, Num);
%% 		_ ->
%% 			cmd({chanmsg, [User#user.nick ++ " sucks huge cock"]}, State),
%% 			ok
%% 	end;
%% parse_cmd(State, _, ["bash" | Rest]) ->
%% 	SearchQuery = string:join(Rest, " "),
%% 	do_bash_search(State, SearchQuery);
%% parse_cmd(State, _, ["gg" | Rest]) ->
%% 	SearchQuery = string:join(Rest, " "),
%% 	do_google(State, SearchQuery);
%% parse_cmd(State, _, ["gc" | Rest]) ->
%% 	SearchQuery = string:join(Rest, " "),
%% 	do_gcalc(State, SearchQuery);
%% parse_cmd(State, _, ["w" | Topic]) ->
%% 	do_wiki(State, "en", string:join(Topic, " "));
%% parse_cmd(State, _, ["в" | Topic]) ->
%% 	do_wiki(State, "ru", string:join(Topic, " "));
%% parse_cmd(State, _, ["l" | Topic]) ->
%% 	do_lurkmore(State, string:join(Topic, " "));
%% parse_cmd(State, _, ["л" | Topic]) ->
%% 	do_lurkmore(State, string:join(Topic, " "));
%% parse_cmd(State, _, ["Jbo" | Sentence]) ->
%% 	do_jbofihe(State, string:join(Sentence, " "));
%% parse_cmd(State, _, ["jbo" | Sentence]) ->
%% 	do_cmafihe(State, string:join(Sentence, " "));
%% parse_cmd(State, _, ["jvo" | Sentence]) ->
%% 	do_jvocuhadju(State, string:join(Sentence, " "));
%% parse_cmd(State, _, ["en-jbo" | Word]) ->
%% 	do_dict(State, "www.lojban.org", "en->jbo", string:join(Word, " "));
%% parse_cmd(State, _, ["en-ru" | Word]) ->
%% 	do_gdict(State, "en|ru", string:join(Word, " "));
%% parse_cmd(State, _, ["ru-en" | Word]) ->
%% 	do_gdict(State, "ru|en", string:join(Word, " "));
%% parse_cmd(State, _, ["de-ru" | Word]) ->
%% 	do_gdict(State, "de|ru", string:join(Word, " "));
%% parse_cmd(State, _, ["ru-de" | Word]) ->
%% 	do_gdict(State, "ru|de", string:join(Word, " "));
%% parse_cmd(State, _, ["jbo-en" | Word]) ->
%% 	do_dict(State, "www.lojban.org", "jbo->en", string:join(Word, " "));
%% parse_cmd(State, _, ["lynch"]) ->
%% 	do_lynch(State, chanmsg);
%% parse_cmd(State, _, ["lynch", "topic"]) ->
%% 	do_lynch(State, topic);
%% parse_cmd(State, _, ["lynchtopic"]) ->
%% 	do_lynch(State, topic);
%% parse_cmd(State, _, ["jabberwock"]) ->
%% 	do_jabberwock(State);
%% parse_cmd(State, _, ["uptime"]) ->
%% 	show_uptime(State);
%% parse_cmd(State, User, ["time"]) ->
%% 	show_time(State, User#user.nick);
%% parse_cmd(State, User, ["ping"]) ->
%% 	show_ping(State, User#user.nick);
%% parse_cmd(State, User, ["stat"]) ->
%% 	show_stat(State, User#user.ident);
%% parse_cmd(State, _, ["help"]) ->
%% 	show_help(State);
%% parse_cmd(State, _, ["dice", Max]) ->
%% 	case catch list_to_integer(Max) of
%% 		{'EXIT', _} ->
%% 			ok;
%% 		X ->
%% 			do_dice(State, X)
%% 	end;
%% parse_cmd(State, User, ["suicide", Delay]) ->
%% 	case catch list_to_integer(Delay) of
%% 		{'EXIT', _} -> 
%% 			ok;
%% 		X ->
%% 			do_suicide(State, User#user.nick, X*1000)
%% 	end;
%% parse_cmd(State, User, ["suicide"]) ->
%% 	do_suicide(State, User#user.nick);
%% parse_cmd(State, User, ["kickme"]) ->
%% 	do_kickme(State, User#user.nick);
%% parse_cmd(State, _, ["identify"]) ->
%% 	cmd({me, ["is twee's bot written in Erlang and Perl (http://tweedle-dee.org/svn/yadbot/)"]}, State),
%% 	ok;
%% parse_cmd(_State, _User, Cmd) ->
%% 	io:format("Unknown command: ~p", [Cmd]),
%% 	unknown_cmd.

%% parse_privcmd(State, User, [?GIVEOP_MAGIC_WORD]) ->
%% 	cmd({mode, [User#user.nick, "+o"]}, State),
%% 	ok;
%% parse_privcmd(State, User, ["stat"]) ->
%% 	show_stat(State, User#user.ident);
%% parse_privcmd(State, User, ["hist"]) ->
%% 	show_history(State, User);
%% parse_privcmd(_State, _User, Cmd) ->
%% 	io:format("Unknown private command: ~p", [Cmd]),
%% 	unknown_cmd.

%% empty_err_msg(X) when X < 0.25 ->
%% 	"А вот хуй...";
%% empty_err_msg(X) when X < 0.5 ->
%% 	"<тут могла бы быть ваша реклама>";
%% empty_err_msg(X) when X < 0.75 ->
%% 	"Да хер его знает.";
%% empty_err_msg(_) ->
%% 	"Почувствуйте себя неудачником!".

%% print_long_results(State, [""]) ->
%% 	cmd({chanmsg, [empty_err_msg(random_p:uniform())]}, State),
%% 	ok;
%% print_long_results(State, Results) ->
%% 	lists:foreach(fun (S) -> cmd({chanmsg, [S]), timer:sleep(?MSG_MIN_DELAY) end, Results}, State),
%% 	ok.	

%% privmsg_long_results(State, Nick, [""]) ->
%% 	cmd({privmsg, [Nick, "--"]}, State);
%% privmsg_long_results(State, Nick, Results) ->
%% 	lists:foreach(fun (S) -> cmd({privmsg, [Nick, S]), timer:sleep(?PRIVMSG_MIN_DELAY) end, Results}, State),
%% 	ok.

%% do_bash(State, Num) ->
%% 	BashCmd = lists:flatten(io_lib:format("~s '~b'", ["bash.pl", Num])),
%% 	{ok, Results} = regexp:split(yadbot_util:system(?SCRIPT_DIR, BashCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_bash_search(State, SearchQuery) ->
%% 	BashCmd = lists:flatten(io_lib:format("~s '~s'", ["bash-search.pl", sanitize(SearchQuery, $')])),  %'
%% 	{ok, Results} = regexp:split(yadbot_util:system(?SCRIPT_DIR, BashCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_google(State, SearchQuery) ->
%% 	GoogleCmd = lists:flatten(io_lib:format("~s '~s'", ["google.pl", sanitize(SearchQuery, $')])),  %'
%% 	{ok, Results} = regexp:split(yadbot_util:system(?SCRIPT_DIR, GoogleCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_gcalc(State, SearchQuery) ->
%% 	GoogleCmd = lists:flatten(io_lib:format("~s '~s'", ["gcalc.pl", sanitize(SearchQuery, $')])),  %'
%% 	{ok, Results} = regexp:split(yadbot_util:system(?SCRIPT_DIR, GoogleCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_wiki(State, Lang, SearchQuery) ->
%% 	WikiCmd = lists:flatten(io_lib:format("~s ~s '~s'", ["wiki.pl", Lang, sanitize(SearchQuery, $')])),  %'
%% 	{ok, Results} = regexp:split(yadbot_util:system(?SCRIPT_DIR, WikiCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_lurkmore(State, Word) ->
%% 	Url = "http://lurkmore.ru/" ++ yadbot_util:uri_encode(Word),
%% 	cmd({me, ["доставил: " ++ Url]}, State).

%% do_jbofihe(State, Sentence) ->
%% 	FiheCmd = lists:flatten(io_lib:format("echo \"~s\" | jbofihe -x", [sanitize(Sentence, $")])),  %"
%% 	{ok, Results} = regexp:split(yadbot_util:system(FiheCmd), "\n"),
%% 	print_long_results(State, Results).
	
%% do_cmafihe(State, Sentence) ->
%% 	FiheCmd = lists:flatten(io_lib:format("echo \"~s\" | cmafihe", [sanitize(Sentence, $")])),  %"
%% 	{ok, Results} = regexp:split(yadbot_util:system(FiheCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_jvocuhadju(State, Sentence) ->
%% 	FiheCmd = lists:flatten(io_lib:format("jvocuhadju ~s", [sanitize(Sentence, $;)])),
%% 	{ok, Results} = regexp:split(yadbot_util:system(FiheCmd), "\n"),
%% 	print_long_results(State, Results).
	
%% do_dict(State, Server, Db, Word) ->
%% 	DictCmd = lists:flatten(io_lib:format("~s -h '~s' -d '~s' \"~s\"", ["dict.rb", Server, Db, sanitize(Word, $")])),  %"
%% 	{ok, Results} = regexp:split(yadbot_util:system(DictCmd), "\n"),
%% 	print_long_results(State, Results).

%% do_gdict(State, LangPair, Word) ->
%% 	GdictCmd = lists:flatten(io_lib:format("~s \"~s\" '~s'", ["gdict.pl", sanitize(Word, $"), LangPair])),  %"
%% 	{ok, Results} = regexp:split(yadbot_util:system(?SCRIPT_DIR, GdictCmd), "\n"),
%% 	print_long_results(State, Results).

%% -define(LYNCH_FILE, "data/lynch.txt").

%% do_lynch(State, Action) ->
%% 	{ok, Data} = file:read_file(?LYNCH_FILE),
%% 	{ok, Lines} = regexp:split(binary_to_list(Data), "\n"),
%% 	LineNo = random_p:uniform(length(Lines)),
%% 	cmd({Action, [lists:nth(LineNo, Lines)]}, State),
%% 	ok.

%% -define(JABBERWOCK_FILE, "data/jabberwock.txt").
%% -define(JABBERWOCK_DELAY, 3000).

%% do_jabberwock(State) ->
%% 	cmd({chanmsg, ["Кхм кхм."]}, State),
%% 	timer:sleep(?JABBERWOCK_DELAY),
%% 	cmd({chanmsg, ["А вот ХУЙ вам, мне лениво."]}, State),
%% 	ok.

%% %%	{ok, Data} = file:read_file(?JABBERWOCK_FILE),
%% %%	{ok, Lines} = regexp:split(binary_to_list(Data), "\n"),
%% %%	lists:foreach(fun (L) -> cmd({chanmsg, [L]), timer:sleep(500) end, Lines}, State),
%% %%	ok.

%% -define(DICE_TIMEOUT, 1000).

%% do_dice(State, Max) ->
%% 	Res = random_p:uniform(Max),
%% 	cmd({chanmsg, ["Кручу, верчу, наебать хочу..."]}, State),
%% 	timer:sleep(?DICE_TIMEOUT),
%% 	cmd({chanmsg, [integer_to_list(Res)]}, State).

%% show_uptime(State) ->
%% 	{Uptime, _} = statistics(wall_clock),
%% 	UptimeSec = Uptime div 1000,
%% 	{Rest1, Sec} = {UptimeSec div 60, UptimeSec rem 60},
%% 	{Rest2, Min} = {Rest1 div 60, Rest1 rem 60},
%% 	{Day, Hour} = {Rest2 div 24, Rest2 rem 24},
%% 	cmd({chanmsg, [lists:flatten(io_lib:format("Uptime: ~b day(s), ~b:~b:~b", [Day, Hour, Min, Sec]))]}, State),
%% 	ok.

%% show_time(State, Nick) ->
%% 	Time = yadbot_util:system("date '+%a %b %d %R:%S %Z %Y'"),
%% 	cmd({chanmsg, [lists:flatten(io_lib:format("Точное время: ~s. Так лучше, ~s?", [Time, Nick]))]}, State),
%% 	ok.

%% show_ping(State, Nick) ->
%% 	{Action, Msg} = ping_msg(Nick, random_p:uniform()),
%% 	cmd({Action, [Msg]}, State).

%% show_help(State) ->	
%% 	cmd({me, ["-- ахуенно полезный и функциональный бот."]}, State),
%% 	cmd({me, ["умеет:"]}, State),
%% 	cmd({chanmsg, ["Команды канала:"]}, State),
%% 	print_long_results(State, ?CHANCMDLIST),
%% 	cmd({chanmsg, ["Приватные команды:"]}, State),
%% 	print_long_results(State, ?PRIVCMDLIST),
%% 	cmd({me, ["няшка =^_^="]}, State).

%% ping_msg(Nick, X) when X < 0.25 ->
%% 	{chanmsg, "Да-да, " ++ Nick ++ "?.."};
%% ping_msg(_Nick, X) when X < 0.5 ->
%% 	{me, "pong"};
%% ping_msg(_Nick, X) when X < 0.75 ->
%% 	{chanmsg, "Ну, понг."};
%% ping_msg(Nick, _) ->
%% 	{chanmsg, Nick ++ ": сам пинг, че надо?"}.

%% stat_line(#userstat{ident = Ident, line_count = LineCount, sym_count = SymCount}) ->
%% 	lists:flatten(io_lib:format(" ~-15s | ~6B | ~8B", [Ident, LineCount, SymCount])).

%% show_stat_line(State, Stat = #userstat{ident = OwnIdent}, OwnIdent) ->
%% 	cmd({chanmsg, [stat_line(Stat) ++ " <=== YOU"]}, State);
%% show_stat_line(State, Stat, _) ->
%% 	cmd({chanmsg, [stat_line(Stat)]}, State).

%% show_stat(State, OwnIdent) ->
%% 	{atomic, Stats} = mnesia:sync_transaction(fun () -> show_stat_txn() end),
%% 	cmd({chanmsg, [" *********** User Stats ************"]}, State),
%% 	cmd({chanmsg, [" Ident           |  Lines |  Symbols"]}, State),
%% 	cmd({chanmsg, [" -----------------------------------"]}, State),
%% 	lists:foreach(fun (X) -> show_stat_line(State, X, OwnIdent), timer:sleep(?MSG_MIN_DELAY) end, Stats),
%% 	cmd({chanmsg, [" ***********************************"]}, State).

%% show_stat_txn() ->
%% 	qlc:eval(qlc:sort(qlc:q([X || X <- mnesia:table(userstat)]), {order, fun (U1, U2) -> U1#userstat.line_count > U2#userstat.line_count end})).

%% show_history(State, User) ->
%% 	H = list_history(State#state.history, User#user.ident, []),
%% 	HL = lists:filter(fun (X) -> length(X) > 0 end, H),
%% 	privmsg_long_results(State, User#user.nick, HL),
%% 	cmd({privmsg, [User#user.nick, "---------------------------"]}, State).

%% list_history([{_, quit, [#user{ident = MyIdent}, _]} | _] = H, Ident, Hist) when MyIdent == Ident ->
%% 	list_history_tail(lists:sublist(H, ?HIST_TAIL), Hist);
%% list_history([{_, part, [#user{ident = MyIdent}, _]} | _] = H, Ident, Hist) when MyIdent == Ident ->
%% 	list_history_tail(lists:sublist(H, ?HIST_TAIL), Hist);
%% list_history([HistEnt | Tail], Ident, Hist) ->
%% 	io:format("Adding histent: ~s~n", [histent_to_list(HistEnt)]),
%% 	list_history(Tail, Ident, [histent_to_list(HistEnt) | Hist]);
%% list_history([], _, Hist) ->
%% 	Hist.

%% list_history_tail([HistEnt | Tail], Hist) ->
%% 	io:format("Adding histent: ~s~n", [histent_to_list(HistEnt)]),
%% 	list_history_tail(Tail, [histent_to_list(HistEnt) | Hist]);
%% list_history_tail([], Hist) ->
%% 	Hist.

%% histent_to_list({Time, Event, Params}) ->
%% 	{_, {HH, MM, _SS}} = Time,
%% 	case histent_to_list({Event, Params}) of
%% 		[] ->
%% 			[];
%% 		X ->
%% 			"[" ++ integer_to_list(HH) ++ ":" ++ integer_to_list(MM) ++ "] " ++ X
%% 	end;

%% histent_to_list({chanmsg, [User, Msg]}) ->
%% 	User#user.nick ++ ": " ++ Msg;
%% histent_to_list({me, [User, Msg]}) ->
%% 	"* " ++ User#user.nick ++ " " ++ Msg;
%% histent_to_list({nick, [User, Nick]}) ->
%% 	User#user.nick ++ " -> " ++ Nick;	
%% histent_to_list({topic, [User, Topic]}) ->
%% 	User#user.nick ++ " TOPIC: " ++ Topic;
%% histent_to_list({join, [User]}) ->
%% 	"===> " ++ User#user.nick;
%% histent_to_list({quit, [User, Reason]}) ->
%% 	"<=== " ++ User#user.nick ++ " ("  ++ Reason ++ ")";
%% histent_to_list({part, [User, Reason]}) ->
%% 	"<=== " ++ User#user.nick ++ " ("  ++ Reason ++ ")";
%% histent_to_list({kick, [Kicker, Nick, Reason]}) ->
%% 	Kicker#user.nick ++ " KICK " ++ Nick ++ " ("  ++ Reason ++ ")";
%% histent_to_list(_) ->
%% 	[].

%% do_suicide(State, Nick, Delay) when (Delay < ?RECONN_MAX_TIMEOUT) and (Delay >= 0) ->
%% 	cmd({kick, [State#state.nick, suicide_msg(Nick, random_p:uniform())]}, State),
%% 	{ok, State#state{reconn_delay = Delay}};
%% do_suicide(State, Nick, _) ->
%% 	cmd({chanmsg, [Nick ++ ": да хуй тебе."]}, State).

%% do_suicide(State, Nick) ->
%% 	do_suicide(State, Nick, ?SUICIDE_TIMEOUT).

%% do_kickme(State, Nick) ->
%% 	cmd({kick, [Nick, goodbye_msg(Nick, random_p:uniform())]}, State),
%% 	ok.

%% do_neko(State) ->
%% 	cmd({chanmsg, ["^_^"]}, State).

%% goodbye_msg(Nick, X) when X < 0.5 -> "Всегда пожалуйста, " ++ Nick;
%% goodbye_msg(_, _) -> "Кто к нам с хуем придет, тот нахуй и пойдет.".

%% suicide_msg(_, X) when X < 0.4 -> "Прощай, жестокий мир.";
%% suicide_msg(Nick, X) when X < 0.8 -> "Сука ты, " ++ Nick ++ "!";
%% suicide_msg(_, _) -> "И ты, Брут :(".

%% suicide_greeting(State, Kicker) when State#state.nick == Kicker -> 
%% 	suicide_self_greeting(State, random_p:uniform());
%% suicide_greeting(_, Kicker) -> ["Ты мудак, " ++ Kicker].

%% suicide_self_greeting(State, X) when X < 0.5 ->
%% 	["Ты суицидальный мудак, " ++ State#state.nick];
%% suicide_self_greeting(_, _) ->
%% 	["Мафия говорила, что она бессмертна..."].

%% comment_topic(State, Nick, _, X) when X < 0.1 ->
%% 	cmd({chanmsg, ["Говенный топег, " ++ Nick ++ "."]}, State);
%% comment_topic(State, _, _, X) when X < 0.2 ->
%% 	cmd({chanmsg, ["Гг :)"]}, State);
%% comment_topic(State, Nick, _, _) ->
%% 	cmd({chanmsg, ["Мощно задвинул, " ++ Nick ++ "."]}, State).

%% comment_join(State, Nick, X) when X < 0.5 ->
%% 	cmd({chanmsg, ["Превед, " ++ Nick ++ "."]}, State);
%% comment_join(State, Nick, X) when X < 0.75 ->
%% 	cmd({chanmsg, [">> ВНИМАНИЕ: К нам приходит пользователь СИСЬКИ^W" ++ Nick ++ ". Поприветствуем!"]}, State);
%% comment_join(State, Nick, _) ->
%% 	cmd({me, ["приветствует " ++ Nick ++ "."]}, State).

%% comment_exit(State, Nick, X) when X < 0.5 ->
%% 	cmd({chanmsg, ["Нам будет нехватать тебя, " ++ Nick ++ "."]}, State);
%% comment_exit(State, _, _) ->
%% 	cmd({chanmsg, ["Гг, наконец-то он ушел."]}, State).

%% update_stat(User, Msg) ->
%% 	mnesia:sync_transaction(fun () -> update_stat_txn(User, Msg) end).

%% update_stat_txn(#user{ident = Ident}, Msg) ->
%% 	case mnesia:read({userstat, Ident}) of
%% 		[] ->
%% 			io:format("New stat~n"),
%% 			ok = mnesia:write(#userstat{ident = Ident, sym_count = length(Msg)});
%% 		[Stat] ->
%% 			io:format("Update stat~n"),
%% 			ok = mnesia:write(Stat#userstat{line_count = Stat#userstat.line_count + 1,
%% 											sym_count = Stat#userstat.sym_count + length(Msg)});
%% 		Any ->
%% 			io:format("WARN: ~p", [Any])
%% 	end.

%% strip_linefeeds([$\r | Tail]) ->
%% 	strip_linefeeds(Tail);
%% strip_linefeeds([$\n | Tail]) ->
%% 	strip_linefeeds(Tail);
%% strip_linefeeds([Char | Tail]) ->
%% 	[Char | strip_linefeeds(Tail) ];
%% strip_linefeeds([]) ->
%% 	[].

%% sanitize(String, $") ->
%% 	{ok, Res, _} = regexp:gsub(String, "\\\"", "'"),
%% 	Res;
%% sanitize(String, $') ->
%% 	{ok, Res, _} = regexp:gsub(String, "\\'", "\""),
%% 	Res;
%% sanitize(String, $;) ->
%% 	{ok, Res, _} = regexp:gsub(String, ";", ""),
%% 	{ok, Res2, _} = regexp:gsub(Res, "&", ""),
%% 	{ok, Res3, _} = regexp:gsub(Res2, "|", ""),
%% 	{ok, Res4, _} = regexp:gsub(Res3, ">", ""),
%% 	{ok, Res5, _} = regexp:gsub(Res4, "<", ""),
%% 	{ok, Res6, _} = regexp:gsub(Res5, "\\(", ""),
%% 	{ok, Res7, _} = regexp:gsub(Res6, "\\)", ""),
%% 	{ok, Res8, _} = regexp:gsub(Res7, "'", "\\'"),
%% 	{ok, Res9, _} = regexp:gsub(Res8, "\"", "\\\""),
%% 	Res9.

