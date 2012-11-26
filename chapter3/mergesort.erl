-module(mergesort).
-author('Patrik Maunus <subscription@skriptladan.se').
-export([merge_sort/1]).

merge([], [], Result) ->
    Result;
merge([], [RightHead|RightTail], Result) ->
    merge([], RightTail, lists:append(Result, [RightHead]));
merge([LeftHead|LeftTail], [], Result) ->
    merge(LeftTail, [], lists:append(Result, [LeftHead]));
merge([LeftHead|LeftTail], [RightHead|RightTail], Result) when LeftHead < RightHead ->
    % Add Left Head to result
    merge(LeftTail, [RightHead|RightTail], lists:append(Result, [LeftHead]));
merge(Left, [RightHead|RightTail], Result) ->
    % Add Right head to result
    merge(Left, RightTail, lists:append(Result, [RightHead])).

merge(Left, Right) ->
    merge(Left, Right, []).

merge_sort(List) when length(List) == 1 ->
    List;
merge_sort(List) ->
    % Use lists:split(N, List1) -> {List2, List3}
    Middle = length(List) / 2,
    {Left, Right} = lists:split(trunc(Middle), List),
    LeftMerge = merge_sort(Left),
    RightMerge = merge_sort(Right),
    merge(LeftMerge, RightMerge).

