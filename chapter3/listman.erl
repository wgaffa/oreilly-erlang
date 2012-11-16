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
	

list_item(L) ->
    list_item(L, []).

list_item([], Result) ->
    reverse(Result);
list_item([H|T], Result) ->
    list_item(T, [H|Result]).
	
concatenate(L) ->
    concatenate(L, []).

concatenate([], Result) ->
    reverse(Result);
concatenate([H|T], Result) ->
    concatenate(T, [H|Result]).

flat_item([]) ->
    [];
flat_item([H|T]) when is_list(H) ->
    flat_item(H) ++ flat_item(T);
flat_item([H|T]) ->
    [H|flat_item(T)].

flatten(L) ->
    flat_item(L).
