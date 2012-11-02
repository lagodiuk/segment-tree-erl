-module(segment_tree_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("segment_tree.hrl").

min_test() ->
	Min = fun
		(X, ?EMPTY_CHILD) -> {min, X}; 
		({min, X}, {min, Y}) -> {min, min(X,Y)}
	end,
	ST = segment_tree:new([1,2,3,4,5,0,7,8,9], Min),
	?assertMatch({min, 1}, segment_tree:fetch(interval(1), ST)),
	?assertMatch({min, 1}, segment_tree:fetch(interval(1,2), ST)),
	?assertMatch({min, 1}, segment_tree:fetch(interval(1,4), ST)),
	?assertMatch({min, 2}, segment_tree:fetch(interval(2,5), ST)),
	?assertMatch({min, 0}, segment_tree:fetch(interval(2,6), ST)),
	?assertMatch({min, 7}, segment_tree:fetch(interval(7,9), ST)),
	?assertMatch({min, 0}, segment_tree:fetch(interval(1,9), ST)).

substr_test() ->
	Concat = fun
		(Char, ?EMPTY_CHILD) -> [Char];
		(Str1, Str2) -> Str1++Str2
	end,
	ST = segment_tree:new("123456789", Concat),
	?assertMatch("1", segment_tree:fetch(interval(1), ST)),
	?assertMatch("123", segment_tree:fetch(interval(1,3), ST)),
	?assertMatch("567", segment_tree:fetch(interval(5,7), ST)),
	?assertMatch("3", segment_tree:fetch(interval(3,3), ST)),
	?assertMatch("56789", segment_tree:fetch(interval(5,9), ST)),
	?assertMatch("89", segment_tree:fetch(interval(8,9), ST)).

get_all_substrings_test() ->
	Concat = fun
        	(Char, ?EMPTY_CHILD) -> [Char];
                (Str1, Str2) -> Str1++Str2
        end,
	ST = segment_tree:new("0123", Concat),
	Substrs = [segment_tree:fetch(interval(X,Y), ST) || X <- lists:seq(1,4), Y <- lists:seq(1,4), X =< Y],
	?assertMatch(["0","01","012","0123","1","12","123","2","23","3"], Substrs).

min_max_sum_test() ->
	MinMaxSum = fun
		(X, ?EMPTY_CHILD) ->
			{{min, X}, {max, X}, {sum, X}};
		({{min, X1}, {max, X2}, {sum, X3}}, {{min, Y1}, {max, Y2}, {sum, Y3}}) ->
			{{min, min(X1,Y1)}, {max, max(X2,Y2)}, {sum, X3+Y3}}
	end,
	ST = segment_tree:new([1,2,3,4,5,6,7,8,9], MinMaxSum),
	?assertMatch({{min, 1}, {max, 1}, {sum, 1}}, segment_tree:fetch(interval(1), ST)),
	?assertMatch({{min, 1}, {max, 3}, {sum, 6}}, segment_tree:fetch(interval(1,3), ST)),
	?assertMatch({{min, 1}, {max, 4}, {sum, 10}}, segment_tree:fetch(interval(1,4), ST)),
	?assertMatch({{min, 2}, {max, 7}, {sum, 27}}, segment_tree:fetch(interval(2,7), ST)),
	?assertMatch({{min, 8}, {max, 9}, {sum, 17}}, segment_tree:fetch(interval(8,9), ST)).

interval(A) ->
	interval:new(A).
interval(A, B) ->
	interval:new(A, B).
