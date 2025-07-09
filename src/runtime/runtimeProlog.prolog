
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['+', A, B]), _, Res) :-
    Res is A + B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['-', A, B]), _, Res) :-
    Res is A - B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['*', A, B]), _, Res) :-
    Res is A * B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['/', A, B]), _, Res) :-
    Res is A / B.

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['>', A, B]), _, Res) :-
    (   A > B
    ->  Z = true
    ;   Z = false
    ).

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['<', A, B]), _, Res) :-
    (   A < B
    ->  Z = true
    ;   Z = false
    ).

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['==', A, B]), _, Res) :-
    (   A == B
    ->  Z = true
    ;   Z = false
    ).

