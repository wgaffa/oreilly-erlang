-module(listman).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).
-author("Patrik Maunus <subscription@skriptladan.se>").

filter([], _) ->
	[];
filter([H|T], Filter) when H =< Filter ->
	[H|filter(T, Filter)];
filter([_|T], Filter) ->
	filter(T, Filter).
	
reverse(L) ->
	reverse(L, []).
	
reverse([], L) ->
	L;
reverse([H|T], L) ->
	reverse(T, [H|L]).
	
concatenate(L) ->
    concatenate(L, []).

concatenate([], Result) ->
    reverse(Result);
concatenate([H|T], Result) ->
    concatenate(T, [H|Result]).

flat_item(L) ->
    flat_item(L, []).

flat_item([], Result) ->
    reverse(Result);
flat_item([H|T], Result) when is_list(H) ->
    flat_item(H, Result);
flat_item([H|T], Result) ->
    flat_item(T, [H|Result]).

flatten(L) ->
    flat_item(L).
