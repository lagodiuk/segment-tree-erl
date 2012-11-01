-module(segment_tree_make).
-export([make/0]).

make() ->
	case make:all() of
		error ->
			error;
		_ ->
			segment_tree_test:test()
	end.
