-module(chapter3).
-compile(export_all).

% Exercise 3-1

% Tail recursion
sum(N) ->
	sum(N, 0).
	
sum(N, Result) when N > 0 ->
	sum(N - 1, Result + N);
sum(0, Result) ->
	Result.
	
% Using direct recursion
sum_direct(N) when N > 0 ->
	N + sum_direct(N - 1);
sum_direct(0) ->
	0.

% Sum numbers between two ranges	
sum_between(N, M) when N < M ->
	N + sum_between(N + 1, M);
sum_between(N, M) when N > M ->
	throw({error, invalid_range});
sum_between(N, N) ->
	N.
	
% Exercise 3-2

reverse_create(N) when N > 0 ->
	[N|reverse_create(N-1)];
reverse_create(0) ->
	[].
	
create(N) when N > 0 ->
	create(1, N).
	
create(N, Boundrary) when N =< Boundrary ->
	[N|create(N + 1, Boundrary)];
create(_, _) ->
	[].
	
% Exercise 3-3

pint(N) when N > 0 ->
	io:format("Number:~p~n", [create(N)]).
	
pint_even(N) ->
	io:format("Number:~p~n", [even(N)]).
	
even(N) when (N rem 2 == 0) and (N > 0) ->
	[N|even(N - 1)];
even(0) ->
	[];
even(N) ->
	even(N-1).