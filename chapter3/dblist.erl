-module(dblist).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).
-author("Patrik Maunus <subscription@skriptladan.se>").

new() ->
    [].

destroy(_Db) ->
    ok.

write(Key, Element, Db) ->
    lists:keystore(Key, 1, Db, {Key, Element}).

delete(Key, Db) ->
    lists:keydelete(Key, 1, Db).

read(Key, Db) ->
    lists:keyfind(Key, 1, Db).

match(Element, Db) ->
    lists:keyfind(Element, 2, Db).


