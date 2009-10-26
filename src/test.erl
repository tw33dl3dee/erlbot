-module(test).

-compile(export_all).

mynow(0) ->
	{erlang:now(), erlang:localtime()};
mynow(HowMany) ->
	erlang:now(),	
	mynow(HowMany - 1).

testnow(HowMany) ->
	io:format("~p~n~p~n", [erlang:now(), erlang:localtime()]),
	R = timer:tc(?MODULE, mynow, [HowMany]),
	timer:sleep(1000),
	{R, {erlang:now(), erlang:localtime()}}.
