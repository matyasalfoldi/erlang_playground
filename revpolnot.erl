-module(revpolnot).
-export([rpn/1]).

%% [10, 4, 3, "+", 2, "*", "-"]

rpn(L) when is_list(L) ->
    rpn(L, []).

apply_fun(F, L) ->
    F(L).

add([First|Second]) ->
    First + Second.

sub([First|Second]) ->
    First - Second.

mul([First|Second]) ->
    First * Second.

divide([First|Second]) ->
    First / Second.

read([H1,H2|T]) ->
    {[H1|H2], T}.

rpn([], Acc) -> Acc;
rpn([H|T], Stack) ->
    if is_number(H) ->
        rpn(T, [H|Stack]);
    true ->
        {Returned, Rest} = read(Stack),
        if H == "+" ->
            Sum = apply_fun(fun add/1, Returned), 
            rpn(T, [Sum|Rest]);
        H == "-" ->
            Sub = apply_fun(fun sub/1, Returned),
            rpn(T, [Sub|Rest]);
        H == "*" ->
            Mul = apply_fun(fun mul/1, Returned),
            rpn(T, [Mul|Rest]);
        H == "/" ->
            Div = apply_fun(fun divide/1, Returned),
            rpn(T, [Div|Rest])
        end
    end.