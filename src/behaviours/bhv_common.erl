%%%-------------------------------------------------------------------
%%% File    : bhv_common.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_common).

-export([empty_check/1, empty_check/2, error/2, fuckoff/2, identify/2]).
-export([pipe_script/4, pipe_script/3]).

-include("irc.hrl").
-include("utf8.hrl").

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

-define(GREETINGS, ["shaking...",
					"liquefying bytes... ",
					"homogenizing goo... ",
					"testing ozone... ",
					"processing... ",
					"spinning violently around the y-axis... ",
					"iodizing... ",
					"stretching images... ",
					"reconstituting sounds... ",
					"faithfully re-imagining... ",
					"scraping funds... ",
					"applying innovation... ",
					"constructing emotional depth... ",
					"debating games as art... ",
					"placating publishers.., ",
					"meticulously diagramming fun... ",
					"filtering moral... ",
					"testing for perfection... ",
					"revolving independence... ",
					"tokenizing innovation... ",
					"self affirming... ",
					"dissolving relationships... ",
					"deterministically simulating the future... ",
					"exceeding cpu quota... ",
					"swapping time and space... ",
					"embiggening prototypes... ",
					"sandbagging expectations... ",
					"challenging everything... ",
					"distilling beauty... ",
					"blitting powers of two... ",
					"manufacturing social responsibility... ",
					"bending the spoon... ",
					"constructing non-linear narrative..."]).

%% Min and max number of greeting messages bot displays on join
-define(MIN_GREETS, 3).
-define(MAX_GREETS, 6).

identify(Chan, short) ->
	irc_conn:action(Chan, nohist, "нядваноль"),
	timer:sleep(500),
	ok = irc_conn:action(Chan, nohist, choice:make(["векторен и гипертекстов",
														 "металлическ и блестящ"]));
identify(Chan, long) ->
	identify(Chan, short),
	irc_conn:action(Chan, nohist, ["обитает по адресу: ", "http://tweedle-dee.org/bzr/erlbot/"]),
	ok = irc_conn:chanmsg(Chan, nohist, ["Советы и предложения постить сюды: ", 
											  "http://redmine.tweedle-dee.org/projects/erlbot/issues/new"]);
identify(Chan, greet) ->
	NumGreets = choice:uniform(?MIN_GREETS, ?MAX_GREETS),
	Greets = [choice:make(?GREETINGS) || _I <- lists:seq(1, NumGreets)],
	ok = irc_conn:bulk_action(Chan, nohist, Greets).
