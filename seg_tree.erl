-module(seg_tree).
-compile(export_all).

-include("interval.hrl").

-record(node, {left, right, val, interval}).
-record(segment_tree, {root, func}).

-define(EMPTY_CHILD, null).
-define(START_INDEX, 1).

node(Left, Right, Val, Interval) ->
        #node{left=Left, right=Right, val=Val, interval=Interval}.
node(Leaf, Val, Interval) ->
        #node{left=Leaf, right=?EMPTY_CHILD, val=Val, interval=Interval}.

stree(List, F) ->
        #segment_tree{root=stree(make_leafs(List, F), [], F), func=F}.

make_leafs(List, F) ->
	make_leafs(List, F, [], ?START_INDEX).
make_leafs([], _F, Acc, _Indx) ->
	lists:reverse(Acc);
make_leafs([H | T], F, Acc, Indx) ->
	Interval = interval:new(Indx),
	make_leafs(T, F, [node(H, F(H, null), Interval) | Acc], Indx+1).

stree([], [Tree], _F) ->
        Tree;
stree([], Acc, F) ->
        stree(lists:reverse(Acc), [], F);
stree([A], Acc, F) ->
        stree([], [A | Acc], F);
stree([A = #node{val=ValA, interval=LInterval, _=_}, B = #node{val=ValB, interval=RInterval, _=_} | T], Acc, F) ->
        stree(T, [node(A, B, F(ValA, ValB), interval:join(LInterval, RInterval)) | Acc], F).

fetch(#interval{left=L, right=R}, SegTree) when R < L ->
	fetch(interval:new(R,L), SegTree);
fetch(Interval, #segment_tree{root=Root, func=F}) ->
	fetch(Interval, Root, F).
fetch(#interval{left=L, right=R}, #node{interval=#interval{left=L, right=R}, val=Val, _=_}, _F) ->
	Val;
fetch(Interval, #node{left=Left, right=Right}, F) ->
	#node{interval=LInterval, _=_} = Left,
	LI = interval:intersect(LInterval, Interval),
	#node{interval=RInterval, _=_} = Right,
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
	
