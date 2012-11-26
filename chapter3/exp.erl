-module(exp).

%% Parse expression like 1+2 into tuples
%% {plus, {num, 1}, {num, 2}}
%% Use Shunting-yard algorithm, lexer (Lexical analysis)
rpn([], Output, []) ->
    Output;
rpn([], Output, Stack) ->
    rpn([], Output ++ Stack, []);
rpn([$+|T], Output, []) ->
    rpn(T, Output, "+");
rpn([$-|T], Output, []) ->
    rpn(T, Output, "-");
rpn([$+|T], Output, [$+|StackTail]) ->
    rpn(T, Output ++ "+", [$+|StackTail]);
rpn([$+|T], Output, [$-|StackTail]) ->
    rpn(T, Output ++ "-", [$+|StackTail]);
rpn([$-|T], Output, [$+|StackTail]) ->
    rpn(T, Output ++ "+", [$-|StackTail]);
rpn([$-|T], Output, [$-|StackTail]) ->
    rpn(T, Output ++ "-", [$-|StackTail]);
rpn(String, Output, Stack) ->
    {Number, Rest} = string:to_integer(String),
    rpn(Rest, Output ++ integer_to_list(Number), Stack).

rpn(String) ->
    rpn(String, [], []).

lexer([$+|T], Result) ->
    lexer(T, [plus|Result]);
lexer([$-|T], Result) ->
    lexer(T, [minus|Result]);
lexer([H|T], Result) ->
    
