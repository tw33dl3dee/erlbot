%%%-------------------------------------------------------------------
%%% File    : bhv_common.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_common).

-export([empty_check/1, empty_check/2, error/2, fuckoff/2]).
-export([pipe_script/4, pipe_script/3]).
-export([is_bot/1]).

-include("irc.hrl").
-include("utf8.hrl").

-define(BOT_NICK_REGEX, "bot$|broom").

is_bot(?USER(Nick)) ->
	erlbot_util:contains(Nick, ?BOT_NICK_REGEX).

%% Execute script from script directory, piping it's output to channel
%% If error occurs, displays error message and output
pipe_script(Chan, Script, Args, Input) ->
	% BUG: use code:priv_dir
	case erlbot_util:execv(Script, Args, "priv/bin", Input) of
		{success, Lines} ->
			ok = irc_conn:bulk_chanmsg(Chan, hist, empty_check(Lines));
		{{failure, ErrCode}, Trace} ->
			Tail = io_lib:format("Exited with ~p", [ErrCode]),
			ok = error(Chan, Trace ++ [Tail])
	end.

pipe_script(Chan, Script, Args) -> pipe_script(Chan, Script, Args, <<>>).

empty_check(Chan, [])   -> irc_conn:chanmsg(Chan, hist, empty_msg());
empty_check(Chan, [""]) -> irc_conn:chanmsg(Chan, hist, empty_msg());
empty_check(_, _)       -> ok.

empty_check([])   -> [empty_msg()];
empty_check([""]) -> [empty_msg()];
empty_check(S)    -> S.

error(Chan, Trace) -> 
	ok = irc_conn:bulk_chanmsg(Chan, hist, [error_msg() | Trace]).

fuckoff(Chan, Nick) ->
	ok = irc_conn:chanmsg(Chan, hist, Nick ++ fuckoff_msg()).

empty_msg() ->
	choice:make(["А вот хуй...",
				 "<тут могла бы быть ваша реклама>",
				 "Да хер его знает.",
				 "Нихуйа не найдено",
				 "Почувствуйте себя неудачником!"]).

fuckoff_msg() ->
	choice:make([", не еби мне моск.", 
				 ", иди нахуй.", 
				 ": да хуй тебе!"]).

error_msg() -> 
	choice:make(["Усе поломалось, насяльника :("]).
