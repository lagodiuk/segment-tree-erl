-module(segment_tree).
-export([new/2, fetch/2]).

-include("interval.hrl").
-include("segment_tree.hrl").

-record(node, {left, right, val, interval}).
-record(segment_tree, {root, func}).

node(Left, Right, Val, Interval) ->
        #node{left=Left, right=Right, val=Val, interval=Interval}.
node(Leaf, Val, Interval) ->
        #node{left=Leaf, right=?EMPTY_CHILD, val=Val, interval=Interval}.

get_interval(#node{interval=Interval, _=_}) ->
	Interval.

new(List, F) ->
	Leafs = make_leafs(List, F),
	Tree = make_tree(Leafs, [], F),
        #segment_tree{root=Tree, func=F}.

make_leafs(List, F) ->
	make_leafs(List, F, [], ?START_INDEX).
make_leafs([], _F, Acc, _Indx) ->
	lists:reverse(Acc);
make_leafs([H | T], F, Acc, Indx) ->
	Interval = interval:new(Indx),
	Value = F(H, ?EMPTY_CHILD),
	Leaf = node(H, Value, Interval),
	make_leafs(T, F, [Leaf | Acc], Indx+1).

make_tree([], [Tree], _F) ->
        Tree;
make_tree([], Acc, F) ->
        make_tree(lists:reverse(Acc), [], F);
make_tree([A], Acc, F) ->
        make_tree([], [A | Acc], F);
make_tree([A = #node{val=ValA, interval=LInterval, _=_}, B = #node{val=ValB, interval=RInterval, _=_} | T], Acc, F) ->
	Interval = interval:join(LInterval, RInterval),
	Value = F(ValA, ValB),
	Node = node(A, B, Value, Interval),
        make_tree(T, [Node | Acc], F).

fetch(#interval{left=L, right=R}, SegTree) when R < L ->
	ReverseInterval = interval:new(R,L),
	fetch(ReverseInterval, SegTree);
fetch(Interval, #segment_tree{root=Root, func=F}) ->
	fetch(Interval, Root, F).
fetch(#interval{left=L, right=R}, #node{interval=#interval{left=L, right=R}, val=Val, _=_}, _F) ->
	Val;
fetch(Interval, #node{left=Left, right=Right}, F) ->
	LInterval = get_interval(Left),
	RInterval = get_interval(Right),
	LI = interval:intersect(LInterval, Interval),
	RI = interval:intersect(Interval, RInterval),
	case {LI, RI} of
		{LI, ?EMPTY_INTERVAL} ->
			fetch(LI, Left, F);
		{?EMPTY_INTERVAL, RI} ->
			fetch(RI, Right, F);
		{LI, RI} ->
			LF = fetch(LI, Left, F),
			RF = fetch(RI, Right, F),
			F(LF, RF)
	end.

%% update(Index, UpdateF, #node{interval=#interval{left=Index, right=Index}, left=X, _=_}, SegTreeF) ->
%%	NewX = UpdateF(X),
%%	NewVal = SegTreeF(NewX),
%%	node(NewX, NewVal, interval(Index)).
%% update(Index, UpdateF, #node{left=Left, right=Right, interval=Interval}, SegTreeF) ->
	
