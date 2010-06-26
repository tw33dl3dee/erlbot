%%%-------------------------------------------------------------------
%%% File    : bhv_common.erl
%%% Author  : Ivan Korotkov <twee@tweedle-dee.org>
%%% Description : 
%%%
%%% Created : 16 Oct 2009 by Ivan Korotkov <twee@tweedle-dee.org>
%%%-------------------------------------------------------------------
-module(bhv_common).

-export([empty_check/1, error/3, fuckoff/3, identify/3]).
-export([pipe_script/5, pipe_script/4]).

-include("irc.hrl").
-include("utf8.hrl").
-include("bhv_common.hrl").

%% Execute script from script directory, piping it's output to channel
%% If error occurs, displays error message and output
pipe_script(Irc, Chan, Script, Args, Input) ->
	case util:execv(Script, Args, ?SCRIPT_DIR, Input) of
		{success, Lines} ->
			ok = irc_conn:bulk_chanmsg(Irc, Chan, empty_check(Lines));
		{{failure, ErrCode}, Trace} ->
			Tail = io_lib:format("Exited with ~p", [ErrCode]),
			ok = error(Irc, Chan, Trace ++ [Tail])
	end.

pipe_script(Irc, Chan, Script, Args) -> pipe_script(Irc, Chan, Script, Args, <<>>).

empty_check([]) -> [empty_msg()];
empty_check([""]) -> [empty_msg()];
empty_check(S) -> S.

empty_msg() ->
	choice:make(["А вот хуй...",
				 "<тут могла бы быть ваша реклама>",
				 "Да хер его знает.",
				 "Нихуйа не найдено",
				 "Почувствуйте себя неудачником!"]).

error(Irc, Chan, Trace) -> 
	ok = irc_conn:bulk_chanmsg(Irc, Chan, [error_msg() | Trace]).

error_msg() -> 
	choice:make(["Усе поломалось, насяльника :("]).

fuckoff(Irc, Chan, Nick) ->
	ok = irc_conn:chanmsg(Irc, Chan, Nick ++ fuckoff_msg()).

fuckoff_msg() ->
	choice:make([", не еби мне моск.", 
				 ", иди нахуй.", 
				 ": да хуй тебе!"]).

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

identify(Irc, Chan, short) ->
	irc_conn:action(Irc, Chan, "нядваноль"),
	timer:sleep(500),
	ok = irc_conn:action(Irc, Chan, choice:make(["векторен и гипертекстов",
												 "металлическ и блестящ"]));
identify(Irc, Chan, long) ->
	identify(Irc, Chan, short),
	irc_conn:action(Irc, Chan, ["обитает по адресу: ", "http://tweedle-dee.org/bzr/erlbot/"]),
	ok = irc_conn:chanmsg(Irc, Chan, ["Советы и предложения постить сюды: ", 
									  "http://redmine.tweedle-dee.org/projects/erlbot/issues/new"]);
identify(Irc, Chan, greet) ->
	NumGreets = choice:uniform(?MIN_GREETS, ?MAX_GREETS),
	Greets = [choice:make(?GREETINGS) || _I <- lists:seq(1, NumGreets)],
	ok = irc_conn:bulk_action(Irc, Chan, Greets).
