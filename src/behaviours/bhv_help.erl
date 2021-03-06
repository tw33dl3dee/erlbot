%%%-------------------------------------------------------------------
%%% File    : bhv_help.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Jan 2010 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_help).

-behaviour(erlbot_behaviour).
-export([init/1, help/1, handle_event/4]).

-include("utf8.hrl").
-include("irc.hrl").

init(_) -> undefined.

help(chancmd) ->
	[{"help",				"вывести вот эту вот бля справку"},
	 {"help list",			"вывести список команд"},
	 {"help mod",			"вывести список модулей поведения"},
	 {"help модуль",		"вывести список команд одного модуля"},
	 {"help !команда",		"вывести справку по публичной команде"},
	 {"help /msg команда",	"вывести справку по приватной команде"}];
help(privcmd) ->
	[{"help ...",			"как !help, но не мозолить глаза остальным обитателям канала"}];
help(about) ->
	"Справка по командам бота".

handle_event(cmdevent, {chancmd, _Chan, ?USER(Nick), ["help" | Query]}, _, _) ->
	show_help(Nick, Query);
handle_event(cmdevent, {privcmd, ?USER(Nick), ["help" | Query]}, _, _) ->
	show_help(Nick, Query);
handle_event(customevent, {end_help, Target}, _, _) ->
	ok = irc_conn:bulk_action(Target, nohist, ["няшка =^_^="]);
handle_event(_Type, _Event, _IrcState, _Data) ->
	not_handled.

show_help(Target, ["list"]) ->	
	irc_conn:bulk_action(Target, nohist, ["-- ахуенно полезный и функциональный бот.", "умеет:"]),
	ShowCmds = fun (?MODULE) -> not_handled;
				   (BhvName) -> show_bhv_help(Target, BhvName) 
			   end,
	[{new_event, specevent, {eval, Target, ShowCmds}, undefined},
	 {new_event, customevent, {end_help, Target}, undefined}];
show_help(Target, ["mod"]) ->
	ShowBhvs = fun (BhvName) -> show_bhv_info(Target, BhvName) end,
	{new_event, specevent, {eval, Target, ShowBhvs}, undefined};
show_help(Target, ["!" ++ CmdName]) ->
	ShowCmd = fun (BhvName) -> show_cmd_info(Target, BhvName, CmdName, chancmd) end,
	{new_event, specevent, {eval, Target, ShowCmd}, undefined};
show_help(Target, ["/msg", CmdName]) ->
	ShowCmd = fun (BhvName) -> show_cmd_info(Target, BhvName, CmdName, privcmd) end,
	{new_event, specevent, {eval, Target, ShowCmd}, undefined};
show_help(Target, [BhvName]) ->
	case catch show_bhv_help(Target, list_to_existing_atom(BhvName)) of
		{'EXIT', _} ->
			ok = irc_conn:chanmsg(Target, hist, unknown_bhv_msg());
		_ ->
			ok
	end;
show_help(Target, _) ->
	show_bhv_help(Target, ?MODULE).

% Show list of behaviour's commands
show_bhv_help(Target, BhvName) ->
	ChanCmds = full_cmd_list(BhvName, chancmd),
	PrivCmds = full_cmd_list(BhvName, privcmd),
	case ChanCmds ++ PrivCmds of
		[]      -> not_handled;
		CmdList ->
			ok = irc_conn:bulk_action(Target, nohist, [bhv_header(BhvName) | tabify(CmdList)])
	end.

%% Show brief behaviour info (`bhv_info')
show_bhv_info(Target, BhvName) ->
	ok = irc_conn:bulk_action(Target, nohist, bhv_info(BhvName)).

%% Find help for specific command
show_cmd_info(Target, BhvName, CmdName, CmdType) ->
	case BhvName:help(CmdType) of
		none    -> not_handled;
		CmdList ->
			L = [irc_conn:bulk_action(Target, nohist, [[cmd_prefix(CmdType), Name, " : ", Help, 
															 " (", atom_to_list(BhvName), ")"]])
				 || {Name, Help} <- CmdList, contains_cmd(Name, CmdName)],
			case length(L) of 0 -> not_handled; _ -> ok end
	end.

%% Header put before each behaviour's commands
bhv_header(BhvName) ->
	case BhvName:help(about) of
		none     -> ["====== ", atom_to_list(BhvName), " ====="];
		BhvDescr -> ["====== ", atom_to_list(BhvName), ": ", BhvDescr, " ====="]
	end.

%% Brief behaviour info: name, about-string, command list
bhv_info(BhvName) ->
	CmdList = brief_cmd_list(BhvName, chancmd) ++ brief_cmd_list(BhvName, privcmd),
	case BhvName:help(about) of
		none     -> [];
		BhvDescr -> [[atom_to_list(BhvName), " : ", BhvDescr, " (", string:join(CmdList, ", "), ")"]]
	end.

%% Commands as list of lines with name and description
full_cmd_list(BhvName, CmdType) ->
	case BhvName:help(CmdType) of
		none -> [];
		List -> [[cmd_prefix(CmdType), Name, " : ", Help] || {Name, Help} <- List]
	end.

%% Commands as list of comma-separated words
brief_cmd_list(BhvName, CmdType) -> 
	case BhvName:help(CmdType) of
		none -> [];
		List -> lists:usort([[cmd_prefix(CmdType), first_word(Cmd)] || {Cmd, _} <- List])
	end.

-define(HELP_TAB_STOP, 4).

tabify(MsgList) ->
	[string:chars($ , ?HELP_TAB_STOP, Msg) || Msg <- MsgList].

cmd_prefix(chancmd) -> "!";
cmd_prefix(privcmd) -> "/msg ".

%% Command may contain multiple choices, split by '|' (en-ru|ru-en)
contains_cmd(Name, CmdName) ->
	lists:member(CmdName, string:tokens(first_word(Name), "|")).

first_word(Str) ->
	hd(string:tokens(Str, " \t\n<>")).

unknown_bhv_msg() ->
	choice:make(["Такого модуля нет. Зато здесь могла бы быть Ваша реклама!",
				 "Дарагой, правильно имя модуля набирать надо, да?",
				 "Такого модуля нет. Возможно, он когда-то был, а потом исчез. "
				   "Или его никогда не было и не будет, и хули ты хочешь -- непонятно."]).
