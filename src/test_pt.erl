-module(test_pt).

-export([parse_transform/2]).

parse_transform(Tree, _Options) ->
	io:format("Tree: ~p, options: ~p~n", [Tree, _Options]),
	parse_node(Tree).

parse_node(Node) when is_list(Node) ->
	[parse_node(Elmt) || Elmt <- Node];
parse_node(Node) when is_tuple(Node) ->
	list_to_tuple([parse_node(Elmt) || Elmt <- tuple_to_list(Node)]);
parse_node(Node) ->
	Node.
