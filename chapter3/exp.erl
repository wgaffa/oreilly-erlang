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

parse(Expression) ->
    parser(lexer(Expression)).

parser(Lex) ->
    {Result, _Rest} = expression(Lex),
    Result.

expression(['~'|T]) ->
    {X, Rest} = expression(T),
    {{'~', X}, Rest};
expression(['('|T]) ->
    {Exp, [')'|Rest]} = bin(T),
    {Exp, Rest};
expression([{num, _} = H|T]) ->
    {H, T}.

bin(Lex) ->
    {X, [Op|T]} = expression(Lex),
    true = lists:member(Op, ['+', '-', '*', '/']),
    {Y, Rest} = expression(T),
    {{Op, X, Y}, Rest}.

lexer([], Result) ->
    lists:reverse(Result);
lexer([$+|T], Result) ->
    lexer(T, ['+'|Result]);
lexer([$-|T], Result) ->
    lexer(T, ['-'|Result]);
lexer([$*|T], Result) ->
    lexer(T, ['*'|Result]);
lexer([$/|T], Result) ->
    lexer(T, ['/'|Result]);
lexer([$(|T], Result) ->
    lexer(T, ['('|Result]);
lexer([$)|T], Result) ->
    lexer(T, [')'|Result]);
lexer([$~|T], Result) ->
    lexer(T, ['~'|Result]);
lexer([$\s|T], Result) ->
    lexer(T, Result); 
lexer([H|_] = L, Result) when H >= $0, H =< $9 ->
    {Number, Rest} = lex_num(L, 0),
    lexer(Rest, [{num, Number}|Result]).

lexer(L) ->
    lexer(L, []).

lex_num([H|T], Number) when H >= $0, H =< $9 ->
    lex_num(T, 10 * Number + H - $0);
lex_num([$.|T], Number) ->
    {Fractal, Rest} = lex_fract(T, 0, 0.1),
    {Number + Fractal, Rest};
lex_num(L, Number) ->
    {Number, L}.

lex_fract([H|T], Number, Fract) when H >= $0, H =< $9 ->
    lex_fract(T, Number + (H - $0) * Fract, Fract/10);
lex_fract(L, Number, _) ->
    {Number, L}.

test() ->
    test_lexer(),
    test_parser(),
    ok.

test_lexer() ->
    [{num, 25}, '+', {num, 30}] = lexer("25+30"),
    [{num, 4.35}] = lexer("4.35"),
    [{num, 65}, '-', {num, 7}, '*', {num, 4}, '/', {num, 2}] = lexer("65 - 7*4 /2"),
    ['(', '(', {num, 2}, '+', {num, 3}, ')', '-', {num, 4}, ')'] = lexer("((2+3)-4)"),
    ['~', '(', '(', {num, 2}, '*', {num, 3}, ')', '+', '(', {num, 3}, '*', {num, 4}, ')', ')'] = lexer("~((2*3)+(3*4))"),
    ok.

test_parser() ->
    {'-', {'+', {num, 2}, {num, 3} }, {num, 4}} = parser(['(', '(', {num, 2}, '+', {num, 3}, ')', '-', {num, 4}, ')']),
    {'~', {'+', {'*', {num, 2}, {num, 3} }, {'*', {num, 3}, {num, 4} } } } = parser(['~', '(', '(', {num, 2}, '*', {num, 3}, ')', '+', '(', {num, 3}, '*', {num, 4}, ')', ')']),
    ok.
