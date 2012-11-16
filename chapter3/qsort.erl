-module(qsort).
-export([qsort/1]).
-author('Patrik Maunus <subscription@skriptladan.se').

qsort(L) when length(L) < 2 ->
    L;
qsort([Pivot|T]) ->
    Lesser = lists:filter(fun(E) ->
				  E < Pivot end, T),
    Greater = lists:filter(fun(E) ->
				   E > Pivot end, T),
    Lesser ++ [Pivot] ++ Greater.
