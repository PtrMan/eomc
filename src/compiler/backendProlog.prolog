% see https://stackoverflow.com/questions/7808766/how-to-check-a-variable-is-a-string-or-a-number-in-prolog
checkIsString(Object) :-
    forall(member(X, Object), number(X)).




count([], 0).
count([_|Tail], Int) :-
    count(Tail, Int__tailCount),
    Int is Int__tailCount + 1.


listStrJoin(List__str__list, Str__seperator,   Str__result) :-
    atomic_list_concat(List__str__list, Str__seperator, Str__result).

% Join list elements with commas into a single atom
listStrJoinComma(List__str__list,   Str__result) :-
	listStrJoin(List__str__list, ',',   Str__result).


strConcat([], '').
strConcat([Str__head|T], Str__dest) :-
    strConcat(T, Str__tail),
    format(string(Str__dest),'~w~w',[Str__head, Str__tail]).

% helper to abstract away formating of string
strFormat(Str_format, Arr__args,   Str__output) :-
    format(string(Str__output), Str_format, Arr__args).




convArgToPrologVarName(Str__argName, Str__SrcPrologVarName) :-
    format(string(Str__SrcPrologVarName), 'VArg__~w', [Str__argName]).
	% V stands for Variable

convArgsToPrologVarNames([], []).
convArgsToPrologVarNames([H|T], [H2|T2]) :-
    convArgToPrologVarName(H, H2),
    convArgsToPrologVarNames(T, T2).









% [0]  MeTTa function name
% [1]  Prolog predicate
% [2]  arity
% [3]  is it a "is" assignment
mettaToPrologFunction('+', '+', 2, true).
mettaToPrologFunction('-', '-', 2, true).
mettaToPrologFunction('*', '*', 2, true).
mettaToPrologFunction('/', '/', 2, true).
mettaToPrologFunction('sqrt', 'sqrt', 1, false).
mettaToPrologFunction('cos', 'cos', 1, false).
mettaToPrologFunction('sin', 'sin', 1, false).
mettaToPrologFunction('exp', 'exp', 1, false).
mettaToPrologFunction('log', 'log', 1, false).

% predicate to emit prolog code for execution of actual AST node
%
% return Prolog source of this AST-node (without recursion)
retPrologSrc(astNode(Str__mettaFnName,[_,_]), Str__Dest, [Str__SrcLeft, Str__SrcRight], Str__OutSrcProlog) :-
    mettaToPrologFunction(Str__mettaFnName, Str__PrologSrcFnName, 2, true), % lookup the Prolog function by MeTTa function
    format(string(Str__OutSrcProlog), '~w is ~w ~w ~w', [Str__Dest, Str__SrcLeft, Str__PrologSrcFnName, Str__SrcRight]).



retPrologSrc(astNode(Str__mettaFnName,[_]), Str__Dest, [Str__Src0], Str__OutSrcProlog) :-
    mettaToPrologFunction(Str__mettaFnName, Str__PrologSrcFnName, 1, false), % lookup the Prolog function by MeTTa function
    format(string(Str__OutSrcProlog), '~w is ~w(~w)', [Str__Dest, Str__PrologSrcFnName, Str__Src0]).











% helper to generate the prolog code of the predicate 
genPrologSrcPredicateHead(Int__PredicateId, Str__SrcProlog__Args, Str__VarnameOutput,   Str__predicateHead) :-
    strFormat('pred~w(runtimeCtx(), runtimeCtx(), ~w, ~w)', [Int__PredicateId, Str__SrcProlog__Args, Str__VarnameOutput],   Str__predicateHead).











emitHelper__letUniversal__genCodeForLetAssignment(Str__destVarname, AstNode__child, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs,  Str__SrcProlog__child,        Str__SrcProlog__letAssignment) :-
    
    % we need to generate the code of the predicate of the child AST-node
    emitPrologFunctionForAst__Recursive(AstNode__child, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__child, Int__PredicateIdChild),
    
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    strFormat('VLet__~w', [Str__destVarname], Str__srcProlog__destVarname),
    
    genPrologSrcPredicateHead(Int__PredicateIdChild, Str__SrcProlog__Args, Str__srcProlog__destVarname,   Str__predicateToCall),
    format(string(Str__SrcProlog__letAssignment), '    ~w, % invoke predicate to do computation of value of let assignment\n', [Str__predicateToCall]),
    
    true.




emitHelper__letUniversal__genCodeForLetAssignments([], ctx(PredIdCounter), ctx(PredIdCounter), _,     '', '').


emitHelper__letUniversal__genCodeForLetAssignments([letAssignment(Str__destVarname, AstNode__child)|Arr__letAssignments], ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs,     Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments) :-
    
	% generate code for assignment of head
    emitHelper__letUniversal__genCodeForLetAssignment(Str__destVarname, AstNode__child, ctx(PredIdCounterIn), ctx(PredIdCounter1), List__EntryPredicateArgs,  Str__SrcProlog__thisChild,        Str__SrcProlog__letAssignmentThis),
    
    % generate code for assignment of tail
    emitHelper__letUniversal__genCodeForLetAssignments(Arr__letAssignments, ctx(PredIdCounter1), ctx(PredIdCounterOut), List__EntryPredicateArgs,     Str__SrcProlog__predicateCallTail, Str__SrcProlog__letAssignmentTail),
    
	% now we need to concat the strings of the generated code
    strConcat([Str__SrcProlog__letAssignmentThis, Str__SrcProlog__letAssignmentTail], Str__SrcProlog__letAssignments),
    strConcat([Str__SrcProlog__thisChild, Str__SrcProlog__predicateCallTail], Str__SrcProlog__predicateCalls),
        
    true.

% manual test
%
% emitHelper__letUniversal__genCodeForLetAssignments([letAssignment('A', astNode(assignConstInt, 10))], ctx(0), ctx(PredIdCounterOut), ['DUMMY'],     Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments).











% we emit every "primitive MeTTa operation" as Prolog functions because this simplifies emitting correct code for matching MeTTa expressions



% emit single prolog function with body of AST expression
%
%
emitPrologFunctionForAst__Recursive(astNode(Str__mettaFnName,[L,R]), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
    
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounter1 is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    % emit code recursivly
    emitPrologFunctionForAst__Recursive(L, ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs, Str__SrcProlog__left, Int__PredicateIdLeft),
    emitPrologFunctionForAst__Recursive(R, ctx(PredIdCounter2), ctx(PredIdCounter3), List__EntryPredicateArgs, Str__SrcProlog__right, Int__PredicateIdRight),
    
    PredIdCounterOut is PredIdCounter3,
    
    
    
    
    % we have to emit the leading Prolog function head
    format(string(T0), 'pred~w(runtimeCtx(), runtimeCtx(), ~w, Res) :-~n', [Int__PredicateIdRes, Str__SrcProlog__Args]),
    %print(T0),
    
    %%%emit(AstNode, ctx(VarIdCounterIn), ctx(VarIdCounterOut), ResId),
    
    %%%retPrologSrc(astNode(Str__mettaFnName,[L,R]), Id__Dest, Id__SrcLeft, Id__SrcRight, Str__OutSrcProlog),
    
    format(string(Str__SrcProlog__invokeLeft), '   pred~w(runtimeCtx(), runtimeCtx(), ~w, T0),~n', [Int__PredicateIdLeft, Str__SrcProlog__Args]),
    format(string(Str__SrcProlog__invokeRight), '   pred~w(runtimeCtx(), runtimeCtx(), ~w, T1),~n', [Int__PredicateIdRight, Str__SrcProlog__Args]),
    
    retPrologSrc(astNode(Str__mettaFnName,[_,_]), 'Res', ['T0', 'T1'], Str__SrcPrologFnCoreCode),
    
    format(string(T1), '   ~w,~n', [Str__SrcPrologFnCoreCode]), % ['Res is T0 + T1']),
    
    format(string(T2), '   true.~n', []),
    
    strConcat([Str__SrcProlog__left, Str__SrcProlog__right, T0, Str__SrcProlog__invokeLeft, Str__SrcProlog__invokeRight, T1, T2], Str__SrcProlog__dest),
    
	true.


% emit single prolog function with body of AST expression
%
%
emitPrologFunctionForAst__Recursive(astNode(Str__mettaFnName,[Arg0]), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
    
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounter1 is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    % emit code recursivly
    emitPrologFunctionForAst__Recursive(Arg0, ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs, Str__SrcProlog__Arg0, Int__PredicateIdArg0),
    
    PredIdCounterOut is PredIdCounter2,
    
    
    
    
    % we have to emit the leading Prolog function head
    format(string(T0), 'pred~w(runtimeCtx(), runtimeCtx(), ~w, Res) :-~n', [Int__PredicateIdRes, Str__SrcProlog__Args]),
    %print(T0),
    
    %%%emit(AstNode, ctx(VarIdCounterIn), ctx(VarIdCounterOut), ResId),
    
    %%%retPrologSrc(astNode(Str__mettaFnName,[L,R]), Id__Dest, Id__SrcLeft, Id__SrcRight, Str__OutSrcProlog),
    
    format(string(Str__SrcProlog__invokeArg0), '   pred~w(runtimeCtx(), runtimeCtx(), ~w, T0),~n', [Int__PredicateIdArg0, Str__SrcProlog__Args]),
    
    retPrologSrc(astNode(Str__mettaFnName,[_]), 'Res', ['T0'], Str__SrcPrologFnCoreCode),
    
    format(string(T1), '   ~w,~n', [Str__SrcPrologFnCoreCode]), % ['Res is T0 + T1']),
    
    format(string(T2), '   true.~n', []),
    
    strConcat([Str__SrcProlog__Arg0, T0, Str__SrcProlog__invokeArg0, T1, T2], Str__SrcProlog__dest),
    
    
	true.



emitPrologFunctionForAst__Recursive(astNode(assignConstInt, Val), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    % we have to emit the leading Prolog predicate head
    format(string(T0), 'pred~w(runtimeCtx(), runtimeCtx(), ~w, Res) :-~n', [Int__PredicateIdRes, Str__SrcProlog__Args]),
    
    format(string(T1), '   Res is ~w,~n', [Val]),
    
    format(string(T2), '   true.~n', []),
    %%%print(T1),
    
    strConcat([T0, T1, T2], Str__SrcProlog__dest),
    
	true.


emitPrologFunctionForAst__Recursive(astNode(retrieveValueByName, Str__Name), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
	
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    
    % TODO : check from where the variable is taken ( MeTTa function declaration arguments, or let inside the function )
    convArgToPrologVarName(Str__Name, Str__SrcProlog__SrcVar),
    
    
    % we have to emit the leading Prolog predicate head
    format(string(Str__predicateHead), 'pred~w(runtimeCtx(), runtimeCtx(), ~w, Res) :-~n', [Int__PredicateIdRes, Str__SrcProlog__Args]),
    
    format(string(Str__0), '   Res = ~w,~n', [Str__SrcProlog__SrcVar]),
    
    format(string(Str__predicateEnd), '   true.~n', []),
    
    
    strConcat([Str__predicateHead, Str__0, Str__predicateEnd], Str__SrcProlog__dest),
    
    true.







% AST-node letUniversal assigns letAsssignment(<VAR DEST NAME>, <SOURCE AST>) before executing <AST child>
% astNode(letUniversal, Arr__letAssignments, AstNode__child)

emitPrologFunctionForAst__Recursive(astNode(letUniversal, Arr__letAssignments, AstNode__child), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
	
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounter1 is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    
    % generate code for let assignments
    emitHelper__letUniversal__genCodeForLetAssignments(Arr__letAssignments, ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs,     Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments),
    
    
    
    genPrologSrcPredicateHead(Int__PredicateIdRes, Str__SrcProlog__Args, 'Res',   Str__generatedPredicate),
    strFormat('~w :-\n', [Str__generatedPredicate], Str__SrcProlog__predicateHead),

    
    % generate and call into code for AstNode__child
    emitPrologFunctionForAst__Recursive(AstNode__child, ctx(PredIdCounter2), ctx(PredIdCounter3), List__EntryPredicateArgs,   Str__SrcProlog__called, Int__PredicateIdCalled),
    
    
    
    genPrologSrcPredicateHead(Int__PredicateIdCalled, Str__SrcProlog__Args, 'Res',   Str__srcProlog__predicateCalled),
    strFormat('   ~w, % invoke child predicate\n', [Str__srcProlog__predicateCalled], Str__SrcProlog__callChildPredicate),
    
    
    Str__SrcProlog__predicateEnd = '   true.\n',
    
    
    
    strConcat([Str__SrcProlog__called, Str__SrcProlog__predicateCalls, Str__SrcProlog__predicateHead, Str__SrcProlog__letAssignments, Str__SrcProlog__callChildPredicate, Str__SrcProlog__predicateEnd], Str__SrcProlog__dest),
    
    PredIdCounterOut is PredIdCounter3,
    
    true.







% AST-node to invoke a metta function
% astNode(invokeFunction, <NAME>, <ARRAY OF AST-NODES OF ARGUMENTS>)

% TODO : generate code for arguments

emitPrologFunctionForAst__Recursive(astNode(invokeFunction, Str__nameOfFunction, Arr__astNodesOfArguments), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
	
    allocatePredicate(PredIdCounterIn, PredIdCounter1, List__EntryPredicateArgs,   Int__PredicateIdRes, Str__srcProlog__predicateHead), % allocate new predicate, generate head of predicate
    
    
   
    strFormat('__~w', [Str__nameOfFunction],   Str__srcProlog__nameOfCalledPredicate), % we need the name of the predicate to invoke
    
    

    %helperForAst__invokeFunction(Arr__astNodesOfArguments,   Str__srcProlog__predicateArgs), % generate name of variables which are unique in this predicate. note that we only care about the number of elements in List Arr__astNodesOfArguments
    
    
    % generate name of variables which are unique in this predicate
    count(Arr__astNodesOfArguments,   Int__n), % count how many elements are in list Arr__astNodesOfArguments
    helperForAst__genArgumentsForInvocationOfPredicate('ResultFromPredicate', List__varnamesOfArguments,  Int__n),
    listStrJoinComma(List__varnamesOfArguments, Str__srcProlog__predicateArgs), % join list of variable names to string
    
    strFormat(' ~w(~w, Res), % invoke predicate of AST-node invokeFunction\n', [Str__srcProlog__nameOfCalledPredicate, Str__srcProlog__predicateArgs], Str__srcProlog__invokePredicate) , % generate code to invoke the predicate
    
    Str__srcProlog__predicateTail = ' true.\n',
    
    strConcat([Str__srcProlog__predicateHead, Str__srcProlog__invokePredicate, Str__srcProlog__predicateTail],   Str__SrcProlog__dest),
    
    
    PredIdCounterOut is PredIdCounter1, % assign counter to output variable
    
    true.






% conditional

emitPrologFunctionForAst__Recursive(astNode(cond, AstNode__Cond, AstNode__TrueCodepath, AstNode__FalseCodepath), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-

	allocatePredicate(PredIdCounterIn, PredIdCounter1, List__EntryPredicateArgs,   Int__PredicateIdRes, Str__srcProlog__predicateHead), % allocate new predicate, generate head of predicate
    
    
    % allocate a predicate for the helper
    allocatePredicate(PredIdCounter1, PredIdCounter2, _,   Int__predicateId__predicateOfHelper, _), % allocate new predicate, generate head of predicate
    
    
    
    emitPrologFunctionForAst__Recursive(AstNode__Cond, ctx(PredIdCounter2), ctx(PredIdCounter3), List__EntryPredicateArgs,   Str__SrcProlog__condition, Int__predicateId__condition),
    
    emitPrologFunctionForAst__Recursive(AstNode__TrueCodepath, ctx(PredIdCounter3), ctx(PredIdCounter4), List__EntryPredicateArgs,   Str__SrcProlog__trueCodepath, Int__predicateId__trueCodepath),
    
    emitPrologFunctionForAst__Recursive(AstNode__FalseCodepath, ctx(PredIdCounter4), ctx(PredIdCounter5), List__EntryPredicateArgs,   Str__SrcProlog__falseCodepath, Int__predicateId__falseCodepath),
    
    strConcat([Str__SrcProlog__condition, Str__SrcProlog__trueCodepath, Str__SrcProlog__falseCodepath],   Str__srcProlog__calledPredicates),
    
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__srcProlog__predicateArgs),
    
    
    
    % generates a helper predicate which does invoke the predicate for the respective code branch
    %
    % ex:
    % pred53(true, runtimeCtx(), runtimeCtx(), VArg__x0) :-
    %  pred5(runtimeCtx(), runtimeCtx(), VArg__x0), % invoke predicate of codepath of true
    %  true.
    %
    % pred53(false, runtimeCtx(), runtimeCtx(), VArg__x0) :-
    %  pred3(runtimeCtx(), runtimeCtx(), VArg__x0), % invoke predicate of codepath of false
    %  true.
    
    %%%Str__srcProlog__predicateArgs = 'TODO',
    
    %%%Int__predicateId__predicateOfHelper = 53,
    %%%Int__predicateId__trueCodepath = 5,
    %%%Int__predicateId__falseCodepath = 3,
    
    strFormat('pred~w(true, runtimeCtx(), runtimeCtx(), ~w) :-\n', [Int__predicateId__predicateOfHelper, Str__srcProlog__predicateArgs], Str__srcProlog__helperTrueCodepath),
    
    strFormat(' pred~w(runtimeCtx(), runtimeCtx(), ~w), % invoke predicate of codepath of true\n true.\n', [Int__predicateId__trueCodepath, Str__srcProlog__predicateArgs], Str__srcProlog__predicatebodyForTrueCodepath),
	
    
    strFormat('pred~w(false, runtimeCtx(), runtimeCtx(), ~w) :-\n', [Int__predicateId__predicateOfHelper, Str__srcProlog__predicateArgs], Str__srcProlog__helperFalseCodepath),
	
    strFormat(' pred~w(runtimeCtx(), runtimeCtx(), ~w), % invoke predicate of codepath of false\n true.\n', [Int__predicateId__falseCodepath, Str__srcProlog__predicateArgs], Str__srcProlog__predicatebodyForFalseCodepath),
    
    
    
    strFormat('\n\n\n~w~w\n\n~w~w', [Str__srcProlog__helperTrueCodepath, Str__srcProlog__predicatebodyForTrueCodepath, Str__srcProlog__helperFalseCodepath, Str__srcProlog__predicatebodyForFalseCodepath],   Str__srcProlog__predicatesForConditionIndirection),
    
    
    
    strConcat([Str__srcProlog__calledPredicates, Str__srcProlog__predicatesForConditionIndirection],   Str__predicatesA),
    
    
    
    % now we generate code for the "real" predicate. This predicate calls into the generated helper predicate after computing the value of the AstNode__Cond AST-node
    
    strFormat(' pred~w(runtimeCtx(), runtimeCtx(), ~w, CondEvalResult), % evalulate value of condition\n', [Int__predicateId__condition, Str__srcProlog__predicateArgs],   Str__srcProlog__evalValueOfCondition),
    strFormat(' pred~w(CondEvalResult, runtimeCtx(), runtimeCtx(), ~w),\n', [Int__PredicateIdRes, Str__srcProlog__predicateArgs],   Str__srcProlog__invokeConditionalHelperPredicate),
    strConcat([Str__srcProlog__evalValueOfCondition, Str__srcProlog__invokeConditionalHelperPredicate, ' true.\n'],   Str__srcProlog__predicateBody),
    
    
    strConcat([Str__srcProlog__predicateHead, Str__srcProlog__predicateBody],   Str__srcProlog__predicate),
    
    
    % emit complete code
    strConcat([Str__predicatesA, Str__srcProlog__predicate],   Str__SrcProlog__dest),
    
    
    PredIdCounterOut is PredIdCounter5,
    
    true.







% allocates a new predicate for the prolog target  and also does some other stuff
allocatePredicate(PredIdCounterIn, PredIdCounterOut, List__EntryPredicateArgs,   Int__PredicateIdRes, Str__srcProlog__predicateHead) :-
	
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    % generate prolog code of head of predicate
    genPrologSrcPredicateHead(Int__PredicateIdRes, Str__SrcProlog__Args, 'Res',   Str__generatedPredicate),
    strFormat('~w :-\n', [Str__generatedPredicate], Str__srcProlog__predicateHead),
    
    
    true.






% helper to generate list of arguments for invocation of predicate in prolog
helperForAst__genArgumentsForInvocationOfPredicate__helper(_, _, [],   0) :- !.

helperForAst__genArgumentsForInvocationOfPredicate__helper(Str__prefix, Int__Count, [Str__a],  1) :-
    strFormat('~w~w', [Str__prefix, Int__Count], Str__a). %,

helperForAst__genArgumentsForInvocationOfPredicate__helper(Str__prefix, Int__Count, [Str__a|List__tail],  Int__CountRemaining) :-
    
    strFormat('~w~w', [Str__prefix, Int__Count], Str__a), %,
    
    Int__CountNext is Int__Count + 1,
    Int__CountRemaining1 is Int__CountRemaining - 1,
    
    helperForAst__genArgumentsForInvocationOfPredicate__helper(Str__prefix, Int__CountNext, List__tail,  Int__CountRemaining1),
    
    !.

helperForAst__genArgumentsForInvocationOfPredicate(Str__prefix, List,  Int__n) :-
    helperForAst__genArgumentsForInvocationOfPredicate__helper(Str__prefix, 0, List, Int__n).

% manual test
% helperForAst__genArgumentsForInvocationOfPredicate('ResultFromPredicate', X,  1).









emitPrologFunctionOfMettaFunctionDefinition(mettaFunctionDefinition(Str__functionname, Int__nArgs, AstNodeBody), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes) :-
	emitPrologFunctionForAst__Recursive(AstNodeBody, ctx(PredIdCounterIn), ctx(PredIdCounter1), List__EntryPredicateArgs, Str__SrcProlog__ofPredicates, Int__PredicateIdCalled),
	
    % build the string of the arguments  based on Int__nArgs
    helperForAst__genArgumentsForInvocationOfPredicate('VArg', List__varnamesOfArguments,  Int__nArgs),
    listStrJoinComma(List__varnamesOfArguments, Str__srcProlog__predicateArgs), % join list of variable names to string
    
    count(List__varnamesOfArguments,   Int__countOfArguments),
    ( Int__countOfArguments > 0 -> Str__srcProlog__predicateArgsComma = ',' ; Str__srcProlog__predicateArgsComma = '' ),
    
    
    strFormat('__~w(runtimeCtx(), runtimeCtx() ~w~w,  Res) :-\n', [Str__functionname, Str__srcProlog__predicateArgsComma, Str__srcProlog__predicateArgs],   Str__srcProlog__head),
    
    % TODO : also take string with let variables into account (which are not bound in the callsite to values, because they are bound in the called let predicate(s))
    strFormat(' pred~w(runtimeCtx(), runtimeCtx() ~w~w,  Res), % invoke predicate which implements body of function written in metta\n', [Int__PredicateIdCalled, Str__srcProlog__predicateArgsComma, Str__srcProlog__predicateArgs],   Str__srcProlog__invokeBodyPredicate),
    
    Str__srcProlog__predicateEnd = ' true.\n',
    
    strConcat([Str__SrcProlog__ofPredicates, '\n', Str__srcProlog__head, Str__srcProlog__invokeBodyPredicate, Str__srcProlog__predicateEnd], Str__SrcProlog__dest),
    
	PredIdCounterOut = PredIdCounter1,
	
	true.




% manual test to look at code generated for condition
manualtest__conditionA :-
    
    
    AstNode0 = astNode(assignConstInt, 1),
    AstNode1 = astNode(assignConstInt, 53),
    AstNode2 = astNode(assignConstInt, 100),
    AstNodeEntry = astNode(cond, AstNode0, AstNode1, AstNode2),
    
    
    
    MettaFunctionDef0 = mettaFunctionDefinition('exampleFunctionA', 2, AstNodeEntry),
    
    
    Ctx0 = ctx(0),
    
    
    % args of the entry predicate
    List__EntryPredicateArgs = ['A', 'B'],
    
    
    %%%emitPrologFunctionForAst__Recursive(AstNodeEntry, Ctx0, Ctx1, List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes),
    
    emitPrologFunctionOfMettaFunctionDefinition(MettaFunctionDef0, Ctx0, Ctx1, List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes),
    
    print(Str__SrcProlog__dest),
    
    true.




main0 :-
    
    
    Ast100 = astNode(assignConstInt, 1),
    
    Ast0 = astNode(assignConstInt, 10),
    Ast1 = astNode(retrieveValueByName, 'A'),
    Ast4 = astNode(letUniversal, [letAssignment('letTestVarZ', Ast100)], Ast1),
    Ast2 = astNode('sqrt',[Ast4]),
%    Ast1 = astNode(assignConstInt, 5),
%    Ast2 = astNode(callFn('exp'), [Ast1]),
	Ast3 = astNode('*',[Ast0,Ast2]),
    
    %%%AstNodeEntry0 = Ast3,
    
    
    
    Ast500 = astNode(invokeFunction, 'exampleInvokedPredicateA', [astNode(assignConstInt, 1), astNode(assignConstInt, 2)]),
    
    AstNodeEntry0 = Ast500,
    
    
    MettaFunctionDef0 = mettaFunctionDefinition('exampleFunctionA', 2, Ast500),
    
    
    Ctx0 = ctx(0),
    
    
    % args of the entry predicate
    List__EntryPredicateArgs = ['A', 'B'],
    
    
    %%%emitPrologFunctionForAst__Recursive(AstNodeEntry0, Ctx0, Ctx1, List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes),
    
    emitPrologFunctionOfMettaFunctionDefinition(MettaFunctionDef0, Ctx0, Ctx1, List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes),
    
    print(Str__SrcProlog__dest),
    

%    
%    
%    
%    emitPrologFunctionWithSingleAstBody(AstNodeEntry0, Ctx0, Ctx1, ResId0),
%    
%    print('~n~n~n'),
%    
%    
%    format(string(Z), 'fn100(a(t)) :-~n', []),
%    print(Z),
%    
%    %format(string(T0), 'T~w is 2.2,~n', [0]),
%    %print(T0),
%
%    %format(string(T1), 'T~w is exp(T~w),~n', [0, 1]),
%    %print(T1),
%    
%    Ast0 = astNode(assignConstInt, 10),
%    Ast1 = astNode(assignConstInt, 5),
%    Ast2 = astNode(callFn('exp'), [Ast1]),
%    AstEntry = astNode('*',[Ast0,Ast2]),
%    
%    CtxIn = ctx(0),
%    emit(AstEntry, CtxIn, CtxOut, ResId),
%    
%    print('true.~n'),
    
true.
















% astExpr(...) is a AST expression, for example mettaExpr([5])



% helper to convert list of AST-expression to list of strings of prolog target code
convAstExprToTargetProlog__helper__convList([], []).
convAstExprToTargetProlog__helper__convList([AstExpr], [Str__srcProlog]) :-
    convAstExprToTargetProlog(AstExpr,   Str__srcProlog).
convAstExprToTargetProlog__helper__convList([AstExpr|List__astTail], [Str__srcProlog|List__str__srcProlog]) :-
    convAstExprToTargetProlog__helper__convList(List__astTail, List__str__srcProlog),
    convAstExprToTargetProlog(AstExpr,   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(astExpr(Val),   Str__srcProlog) :-
    number(Val), % Val must be a number
    strFormat('~w', [Val],   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(astExpr(Str),   Str__srcProlog) :-
    checkIsString(Str), % Val must be a string
    strFormat('\'~w\'', [Str],   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(astExpr(List__inner),   Str__srcProlog) :-
    
    convAstExprToTargetProlog__helper__convList(List__inner, List__str__srcPrologInner),
    listStrJoin(List__str__srcPrologInner, ',',   Str__strProlog__inner),
    
    strFormat('mettaExpr([~w])', [Str__strProlog__inner],   Str__srcProlog),
    
    !,
    
    true.

% manual test
%    convAstExprToTargetProlog(astExpr([astExpr('a'), astExpr(5)]),   Str).




















%%%%%%
%
% TODO : overhaul the code generation so that MeTTa  declarations which violate the prefix assumption are handlded: (= ($A (test0))   10)
%
% with the following target code:
% 
% dispatch(runtimeCtx(), runtimeCtx(), mettaExpr([A, mettaExpr(['test0'])]), Res) :-
%     Res = 10,
%     true.
%
%
% we also need to generate code to handle any call site (recursivly at runtime). This costs speed but is necessary for some MeTTa code
%
% dispatch for '+'
% dispatch(runtimeCtx(), runtimeCtx(), mettaExpr(['+', VArg__A, VArg__B]), Res) :-
%     dispatch(runtimeCtx(), runtimeCtx(), VArg__A), Res_0),
%     dispatch(runtimeCtx(), runtimeCtx(), VArg__B), Res_1),
%     Res is Res_0 + Res1,
%     true.

