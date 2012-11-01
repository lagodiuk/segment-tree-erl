-module(interval).
-export([new/1, new/2]).
-export([join/2, intersect/2, contains/2]).

-include("interval.hrl").

%% create interval which represents single point
new(X) ->
	new(X, X).

%% create interval which has left point smaller than right
new(L, R) when L =< R ->
	#interval{left=L, right=R};
new(R, L) ->
	#interval{left=L, right=R}.

%% join "neighbour" intervals:
%% [x..(n-1)] + [n..y] -> [x..y]
join(#interval{left=L1, right=R1}, #interval{left=L2, right=R2}) when (L2-R1) == 1 ->
	new(L1, R2).

%% if intervals intersect - returns intersection interval
%% otherwise - returns EMPTY_INTERVAL (which is defined in "interval.hrl")
intersect(Interval, Interval) ->
	Interval;
intersect(#interval{left=L1, right=R1}, #interval{left=L2, right=R2}) ->
	A0 = [L1, R1, L2, R2],
	A1 = lists:sort(A0),
	[_, X1, X2, _] = A1,
	case A0 == A1 of
		false ->
			new(X1, X2);
		true when X1 == X2 ->
			new(X1, X2);
		true ->
			?EMPTY_INTERVAL
	end.

%% check if interval contains Val
contains(#interval{left=L, right=R}, Val) when (L =< R) and (L =< Val) and (Val =< R) ->
	true;
contains(#interval{left=L, right=R}, Val) when (R =< L) and (R =< Val) and (Val =< L) ->
	true;
contains(_, _) ->
	false.
