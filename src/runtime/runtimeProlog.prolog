
pred__+(runtimeCtx(), runtimeCtx(), A, B, Res) :-
    Res is A + B.
pred__-(runtimeCtx(), runtimeCtx(), A, B, Res) :-
    Res is A - B.
pred__*(runtimeCtx(), runtimeCtx(), A, B, Res) :-
    Res is A * B.
pred__/(runtimeCtx(), runtimeCtx(), A, B, Res) :-
    Res is A / B.

pred__gt(A, B, Z) :-
    (   A > B
    ->  Z = true
    ;   Z = false
    ).

pred__lt(A, B, Z) :-
    (   A < B
    ->  Z = true
    ;   Z = false
    ).

pred__eq(A, B, Z) :-
    (   A == B
    ->  Z = true
    ;   Z = false
    ).

