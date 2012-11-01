-module(seg_tree).
-compile(export_all).

-record(interval, {left, right}).
-record(node, {left, right=null, val, interval}).
-record(segment_tree, {root, func}).

-define(START_INDEX, 1).

interval(X) ->
	interval(X, X).
interval(L, R) ->
	#interval{left=L, right=R}.

join(#interval{left=L1, right=R1}, #interval{left=L2, right=R2}) when (L2-R1) == 1 ->
	interval(L1, R2).

intersect(Interval, Interval) ->
	Interval;
intersect(#interval{left=L1, right=R1}, #interval{left=L2, right=R2}) ->
	A0 = [L1, R1, L2, R2],
	A1 = lists:sort(A0),
	[_, X1, X2, _] = A1,
	case A0 == A1 of
		false ->
			interval(X1, X2);
		true when X1 == X2 ->
			interval(X1, X2);
		true ->
			null
	end.

contains(#interval{left=L, right=R}, Val) when (L =< Val) and (Val =< R) ->
	true;
contains(_, _) ->
	false.

node(Left, Right, Val, Interval) ->
        #node{left=Left, right=Right, val=Val, interval=Interval}.
node(Leaf, Val, Interval) ->
        #node{left=Leaf, val=Val, interval=Interval}.

stree(List, F) ->
        #segment_tree{root=stree(make_leafs(List, F), [], F), func=F}.

make_leafs(List, F) ->
	make_leafs(List, F, [], ?START_INDEX).
make_leafs([], _F, Acc, _Indx) ->
	lists:reverse(Acc);
make_leafs([H | T], F, Acc, Indx) ->
	Interval = interval(Indx),
	make_leafs(T, F, [node(H, F(H, null), Interval) | Acc], Indx+1).

stree([], [Tree], _F) ->
        Tree;
stree([], Acc, F) ->
        stree(lists:reverse(Acc), [], F);
stree([A], Acc, F) ->
        stree([], [A | Acc], F);
stree([A = #node{val=ValA, interval=LInterval, _=_}, B = #node{val=ValB, interval=RInterval, _=_} | T], Acc, F) ->
        stree(T, [node(A, B, F(ValA, ValB), join(LInterval, RInterval)) | Acc], F).

fetch(#interval{left=L, right=R}, SegTree) when R < L ->
	fetch(interval(R,L), SegTree);
fetch(Interval, #segment_tree{root=Root, func=F}) ->
	fetch(Interval, Root, F).
fetch(#interval{left=L, right=R}, #node{interval=#interval{left=L, right=R}, val=Val, _=_}, _F) ->
	Val;
fetch(Interval, #node{left=Left, right=Right}, F) ->
	#node{interval=LInterval, _=_} = Left,
	LI = intersect(LInterval, Interval),
	#node{interval=RInterval, _=_} = Right,
	RI = intersect(Interval, RInterval),
	case {LI, RI} of
		{LI, null} ->
			fetch(LI, Left, F);
		{null, RI} ->
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
	
