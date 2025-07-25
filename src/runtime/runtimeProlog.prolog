pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['+', A, B]),  Res) :-
    Res is A + B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['-', A, B]),  Res) :-
    Res is A - B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['*', A, B]),  Res) :-
    Res is A * B.
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['/', A, B]),  Res) :-
    Res is A / B.

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['cos', Arg]),  Res) :-
    Res is cos(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['sin', Arg]),  Res) :-
    Res is sin(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['exp', Arg]),  Res) :-
    Res is exp(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['log', Arg]),  Res) :-
    Res is log(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['tan', Arg]),  Res) :-
    Res is tan(Arg).
pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['tanh', Arg]),  Res) :-
    Res is tanh(Arg).

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['>', A, B]),  Res) :-
    (   A > B
    ->  Z = true
    ;   Z = false
    ).

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['<', A, B]),  Res) :-
    (   A < B
    ->  Z = true
    ;   Z = false
    ).

pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['==', A, B]),  Res) :-
    (   A == B
    ->  Z = true
    ;   Z = false
    ).












% return item at index
listRetAtIdx([], 0,   nil).
listRetAtIdx([Item|_], 0,   Item).

listRetAtIdx([_|Tail], Int__Idx, Res) :-
    Int__Idx2 is Int__Idx - 1,
    listRetAtIdx(Tail, Int__Idx2, Res),
    !.


listRetFirst([],  nil).
listRetFirst([Val|_],   Val).

listRetCdr([],  nil).
listRetCdr([_],  nil).
listRetCdr([_|Val],   Val).





% MeTTa standard library
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['index-atom', metaExpr(List), Int__Idx]),    Res) :-
    listRetAtIdx(List, Int__Idx,   Res).


% MeTTa stdlib
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['car-atom', metaExpr(List)]),   Res) :-
    listRetFirst(List,   Res).

% MeTTa stdlib
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['cdr-atom', metaExpr(List)]),   Res) :-
    listRetCdr(List,   Res).


% function for unittests for checking if compiler produced correct code which leads to correct result
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['asserteq2', A, B]),   Res) :-
    Z0 is A - B,
    Z1 is abs(Z0),

    ( Z1 < 0.001 ->
        true
    ;
        halt(1) % terminate with error return code
    ),
    
    Res = true,
    
    true.


% function for printing for various purposes
% is not part of the metta-stdlib
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['writeln2', Expr]),    Res) :-
    write(Expr),
    write("\n"),
    true.






% MeTTa superpose()
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['superpose', mettaExpr(List)]),  Res) :-
    % superpose takes a expression in MeTTa and converts it nondeterministic controlflow and dataflow.
    %
    % we are using the predicate member/2 to do this in Prolog.
    member(Res, List).

% manual-test with
% :- pred__(a, a, mettaExpr(['superpose', mettaExpr([a,b])]), [], Res).


% MeTTa sequence()
pred__(RuntimeCtx, RuntimeCtx, mettaExpr(['sequence'|List__argsTail]),  Res) :-
    % here we aren't doing anything except to retturn the last argumen

    last(List__argsTail, Res).
