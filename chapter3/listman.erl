-module(listman).
-compile(export_all).
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
	
list_item([]) ->
	[];
list_item([H|T]) ->
	[H|list_item(T)].
	
concatenate([]) ->
	[];
concatenate([H|T]) ->
	list_item(H) ++ concatenate(T).
	