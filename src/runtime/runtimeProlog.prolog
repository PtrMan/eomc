
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['+', A, B]), _, Res) :-
    Res is A + B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['-', A, B]), _, Res) :-
    Res is A - B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['*', A, B]), _, Res) :-
    Res is A * B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['/', A, B]), _, Res) :-
    Res is A / B.

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['cos', Arg]), _, Res) :-
    Res is cos(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['sin', Arg]), _, Res) :-
    Res is sin(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['exp', Arg]), _, Res) :-
    Res is exp(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['log', Arg]), _, Res) :-
    Res is log(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['tan', Arg]), _, Res) :-
    Res is tan(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['tanh', Arg]), _, Res) :-
    Res is tanh(Arg).

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

