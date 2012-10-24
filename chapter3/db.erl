-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-author("Patrik Maunus <subscription@skriptladan.se>").

% Create a new Db
new() ->
	[].
	
% Deletes the Db
destroy(_Db) ->
	ok.
	
% Write a new Key/Value pair
% Any existing keys are overwritten
write(Key, Element, Db) ->
	% Delete old key
	New_db = delete(Key, Db),
	[{Key, Element}|New_db].
	
% Delete Key from Db
delete(_, []) -> [];
delete(Key, [{K,_}|T]) when K == Key ->
	T;
delete(Key, [H|T]) ->
	[H|delete(Key, T)].

% Retrieve a Value from Db
read(_, []) ->
	{error, instance};
read(Key, [{K,V}|_]) when K == Key ->
	{ok, V};
read(Key, [_|T]) ->
	read(Key, T).
	
% Retrieve Keys that have Element
match(_, []) ->
	[];
match(Element, [{K,V}|T]) when V == Element ->
	[K|match(Element, T)];
match(Element, [H|T]) ->
	match(Element, T).
	