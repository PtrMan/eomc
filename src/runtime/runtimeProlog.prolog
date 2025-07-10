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




% MeTTa superpose()
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['superpose', mettaExpr(List)]), _, Res) :-
    % superpose takes a expression in MeTTa and converts it nondeterministic dataflow.
    %
    % we are using the predicate member/2 to do this in Prolog.
    member(Res, List).

% manual-test with
% :- pred__(a, a, mettaExpr(['superpose', mettaExpr([a,b])]), [], Res).

