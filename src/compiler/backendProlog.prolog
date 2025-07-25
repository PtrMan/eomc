
% see https://stackoverflow.com/questions/7808766/how-to-check-a-variable-is-a-string-or-a-number-in-prolog
checkIsString(Object) :-
    forall(member(X, Object), number(X)).

% see https://stackoverflow.com/questions/64976271/is-there-a-way-to-check-if-an-element-is-boolean-in-prolog
checkBoolean(Val) :-
    Val == true; Val == false.


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

zipListStrWithPrefix([], _,   []).
zipListStrWithPrefix([Str__head__varname|List__tailOf__varnames], Str__prefix,   [Str__result|List__varnamesWithPrefixes]) :-
    zipListStrWithPrefix(List__tailOf__varnames, Str__prefix,   List__varnamesWithPrefixes),
    strConcat([Str__prefix, Str__head__varname],   Str__result).





% set manipulation

set_contains([Val|_], Val,   true) :-
    !. % cut because we returned value
set_contains([_|Tail], Val,   Res) :-
    set_contains(Tail, Val,   Res).
set_contains([], _,   false).

% put without checking if item is already in set
set_put(Val, List,   [Val|List]).
















convArgToPrologVarName(Str__argName, Str__SrcPrologVarName) :-
    format(string(Str__SrcPrologVarName), 'VArg__~w', [Str__argName]).
	% V stands for Variable

convArgsToPrologVarNames([], []).
convArgsToPrologVarNames([H|T], [H2|T2]) :-
    convArgToPrologVarName(H, H2),
    convArgsToPrologVarNames(T, T2).







/* commented because not used

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


 */






% for logging tracing of compilation stuff

% /inparam Str__uniqueName unique name for a human. used to make sense of the same codeblock/functionality
% /inparam Str__message
logCompileTrace(Str__uniqueName, Str__message) :-
    strConcat(["compile trace: ", Str__uniqueName, " - ", Str__message],   Str__full),

    write(Str__full),
    write("\n"),
    true.






% helper to generate the prolog code of the predicate 
genPrologSrcPredicateHead(Int__PredicateId, Str__SrcProlog__Args, Str__srcProlog__letAssignments, Str__VarnameOutput,   Str__predicateHead) :-
    strFormat('pred~w(runtimeCtx(), runtimeCtx(), ~w, ~w, ~w)', [Int__PredicateId, Str__SrcProlog__Args, Str__srcProlog__letAssignments, Str__VarnameOutput],   Str__predicateHead).




% convert list of variablenames to string with comma string
convListOfVariablesAndPrefixToStringAndComma(List__varnames, Str__prefix,    Str__srcProlog__predicateArgs, Str__srcProlog__predicateArgsComma) :-

    count(List__varnames,   Int__n), % count how many elements are in list
    zipListStrWithPrefix(List__varnames, Str__prefix,   List__varnamesWithPrefixes),
    
    listStrJoinComma(List__varnamesWithPrefixes, Str__srcProlog__predicateArgs), % join list of variable names to string
    
    ( Int__n > 0 -> Str__srcProlog__predicateArgsComma = ',' ; Str__srcProlog__predicateArgsComma = '' ).












% allocates a new predicate for the prolog target  and also does some other stuff
allocatePredicate(PredIdCounterIn, PredIdCounterOut, List__EntryPredicateArgs, Set__Str__letVariableNames,     Int__PredicateIdRes, Str__srcProlog__predicateHead) :-
	
    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % build the string and stuff of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix, Str__srcProlog__letVariables), % join list of variable names to string

    strFormat("[~w]", [Str__srcProlog__letVariables], Str__srcProlog__let),

    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    
    % generate prolog code of head of predicate
    genPrologSrcPredicateHead(Int__PredicateIdRes, Str__SrcProlog__Args, Str__srcProlog__let, 'Res',   Str__generatedPredicate),
    strFormat('~w :-\n', [Str__generatedPredicate], Str__srcProlog__predicateHead),
    
    
    true.











emitHelper__letUniversal__genCodeForLetAssignment(Str__destVarname, AstNode__child, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__child,        Str__SrcProlog__letAssignment) :-
    
    % build the string of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string


    strFormat("[~w]", [Str__srcProlog__letVariables],   Str__srcProlog__letVariablesAsList),


    % we need to generate the code of the predicate of the child AST-node
    emitPrologFunctionForAst__Recursive(AstNode__child, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,   Str__SrcProlog__child, Int__PredicateIdChild),
    
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    
    strFormat('VLet__~w', [Str__destVarname], Str__srcProlog__destVarname),
    
    genPrologSrcPredicateHead(Int__PredicateIdChild, Str__SrcProlog__Args, Str__srcProlog__letVariablesAsList, Str__srcProlog__destVarname,   Str__predicateToCall),
    format(string(Str__SrcProlog__letAssignment), '    ~w, % invoke predicate to do computation of value of let assignment\n', [Str__predicateToCall]),
    
    true.




emitHelper__letUniversal__genCodeForLetAssignments([], ctx(PredIdCounter), ctx(PredIdCounter), _, _, _,   '', '').


%%%OUTDATED  emitHelper__letUniversal__genCodeForLetAssignments([letAssignment(Str__destVarname, AstNode__child)|Arr__letAssignments], ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs,     Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments) :-
emitHelper__letUniversal__genCodeForLetAssignments(
    [decoratedMettaExpr(nil,[var(Str__destVarname),AstNode__child])|Arr__letAssignments],
    ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,        Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments
) :-

	% generate code for assignment of head
    emitHelper__letUniversal__genCodeForLetAssignment(Str__destVarname, AstNode__child, ctx(PredIdCounterIn), ctx(PredIdCounter1), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__thisChild,        Str__SrcProlog__letAssignmentThis),
    
    % generate code for assignment of tail
    emitHelper__letUniversal__genCodeForLetAssignments(Arr__letAssignments, ctx(PredIdCounter1), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__predicateCallTail, Str__SrcProlog__letAssignmentTail),
    
	% now we need to concat the strings of the generated code
    strConcat([Str__SrcProlog__letAssignmentThis, Str__SrcProlog__letAssignmentTail], Str__SrcProlog__letAssignments),
    strConcat([Str__SrcProlog__thisChild, Str__SrcProlog__predicateCallTail], Str__SrcProlog__predicateCalls),
        
    true.

% manual test
%
% ?- Ast = [decoratedMettaExpr(nil,[var('a'),3])], emitHelper__letUniversal__genCodeForLetAssignments(Ast, ctx(0), ctx(PredIdCounterOut), ['DUMMY'], ['DUMMY2']    Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments).
% Ast = [decoratedMettaExpr(nil, [var(a), 3])],
% PredIdCounterOut = 1,
% Str__SrcProlog__predicateCalls = "pred0(runtimeCtx(), runtimeCtx(), VArg__DUMMY, Res) :-\n   Res is 3,\n   true.\n",
% Str__SrcProlog__letAssignments = "    pred0(runtimeCtx(), runtimeCtx(), VArg__DUMMY, VLet__a), % invoke predicate to do computation of value of let assignment\n".










% we emit every "primitive MeTTa operation" as Prolog functions because this simplifies emitting correct code for matching MeTTa expressions




% emission of code for number const or boolean const
emitPrologFunctionForAst__Recursive(Val, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, PredIdCounterIn) :-
    write("compile trace: (condition) GENconstA check condition\n"),
    
    ( number(Val) ; checkBoolean(Val) ), % Val must be a number or boolean!

    write("compile trace: (condition) GENconstA success\n"),

    %%%Int__PredicateIdRes = PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc,   Str__SrcProlog__Args),
    
    
    % we have to emit the leading Prolog predicate head
    strFormat("pred~w(runtimeCtx(), runtimeCtx(), ~w, _,   Res) :-~n", [PredIdCounterIn, Str__SrcProlog__Args],   T0),
    
    ( number(Val)
    ->  strFormat("   Res is ~w.~n", [Val],   T1)
    ;   strFormat("   Res = ~w.~n", [Val],   T1)
    ),
    
    strConcat([T0, T1],   Str__SrcProlog__dest),
    
	true.

% manual test
%  emitPrologFunctionForAst__Recursive(astNode(assignConstInt, 2), ctx(0), ctx(PredIdCounterOut), [], [],    Str__srcProlog__predicate, Int__PredicateIdRes).






% emission of code for atom
emitPrologFunctionForAst__Recursive(Val, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, PredIdCounterIn) :-
    
    write("compile trace: (condition) GENatom check condition\n"),
    
    atom(Val), % Val must be a atom!

    write("compile trace: (condition) GENatom success\n"),

    %%%Int__PredicateIdRes = PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc,   Str__SrcProlog__Args),
    
    
    % we have to emit the leading Prolog predicate head
    strFormat("pred~w(runtimeCtx(), runtimeCtx(), ~w, _,   Res) :-~n", [PredIdCounterIn, Str__SrcProlog__Args],   T0),
    
    strFormat("   Res = ~w.~n", [Val],   T1),
    
    strConcat([T0, T1],   Str__SrcProlog__dest),
    
	true.



emitPrologFunctionForAst__Recursive(var(Str__Name), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,  Str__SrcProlog__dest, Int__PredicateIdRes) :-
	
    % build the string of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string

    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounterOut is PredIdCounterIn + 1,
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),

    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),


    
    % check from where the variable is taken ( MeTTa function declaration arguments, or let inside the function )
    set_contains(Set__Str__letVariableNames, Str__Name,   Bool__isLetVariable),
    !,
    ( Bool__isLetVariable
    ->  Str__varPrefix = "VLet__"
    ;   Str__varPrefix = "VArg__"
    ),
    strConcat([Str__varPrefix, Str__Name],   Str__SrcProlog__SrcVar),


    
    % we have to emit the leading Prolog predicate head
    strFormat("pred~w(runtimeCtx(), runtimeCtx(), ~w, [~w], Res) :-~n", [Int__PredicateIdRes, Str__SrcProlog__Args, Str__srcProlog__letVariables],   Str__predicateHead),
    
    strFormat(" Res = ~w.~n", [Str__SrcProlog__SrcVar],   Str__0),    
    
    strConcat([Str__predicateHead, Str__0], Str__SrcProlog__dest),
    
    true.















% AST-node let assigns let asssignment(<VAR DEST NAME>, <SOURCE AST>) before executing <AST child>
emitPrologFunctionForAst__Recursive(
    decoratedMettaExpr(let2,['let2',decoratedMettaExpr(nil,List__AstNode__assignments),AstNode__child]),
    ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,   Str__SrcProlog__dest, Int__PredicateIdRes
) :-


    % build the string of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string

    strFormat("[~w]", [Str__srcProlog__letVariables],   Str__srcProlog__letVariablesAsList),


    Int__PredicateIdRes is PredIdCounterIn, % assign id of generated prolog predicate
    PredIdCounter1 is PredIdCounterIn + 1,
    
    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
    
    % concat
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__SrcProlog__Args),
    

    format("trace: codegen for let*\n"),
    

    % generate code for let assignments
    emitHelper__letUniversal__genCodeForLetAssignments(List__AstNode__assignments, ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,      Str__SrcProlog__predicateCalls, Str__SrcProlog__letAssignments),
    

    print(Str__SrcProlog__predicateCalls),nl,
    print(Str__SrcProlog__letAssignments),nl,

    
    genPrologSrcPredicateHead(Int__PredicateIdRes, Str__SrcProlog__Args, Str__srcProlog__letVariablesAsList, 'Res',   Str__generatedPredicate),
    strFormat('~w :-\n', [Str__generatedPredicate], Str__SrcProlog__predicateHead),

    
    % generate and call into code for AstNode__child
    emitPrologFunctionForAst__Recursive(AstNode__child, ctx(PredIdCounter2), ctx(PredIdCounter3), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__called, Int__PredicateIdCalled),
    
    
    
    genPrologSrcPredicateHead(Int__PredicateIdCalled, Str__SrcProlog__Args, Str__srcProlog__letVariablesAsList, 'Res',   Str__srcProlog__predicateCalled),
    strFormat('   ~w, % invoke child predicate\n', [Str__srcProlog__predicateCalled], Str__SrcProlog__callChildPredicate),
    
    
    Str__SrcProlog__predicateEnd = '   true.\n',
    
    
    
    strConcat([Str__SrcProlog__called, Str__SrcProlog__predicateCalls, Str__SrcProlog__predicateHead, Str__SrcProlog__letAssignments, Str__SrcProlog__callChildPredicate, Str__SrcProlog__predicateEnd], Str__SrcProlog__dest),
    
    PredIdCounterOut is PredIdCounter3,
    
    true.









% collapse/1() MeTTa function needs special treatment.
% reason is that the outgoing dataflow depends on nondeterministic computation in the child
emitPrologFunctionForAst__Recursive(decoratedMettaExpr(invokeFunction(_),['collapse', AstNode__child]), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, Int__PredicateIdRes) :-

    write("compile trace: ENTER invokeCollapse\n"),

    !, % drop all other backtracking

    % build the string and stuff of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, "VLet__",   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string


	allocatePredicate(PredIdCounterIn, PredIdCounter1, List__EntryPredicateArgs, Set__Str__letVariableNames,   Int__PredicateIdRes, Str__srcProlog__predicateHead), % allocate new predicate, generate head of predicate
    
    
    
    
    emitPrologFunctionForAst__Recursive(AstNode__child, ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__invoked, Int__predicateId__invoked),
    



    
    convArgsToPrologVarNames(List__EntryPredicateArgs, List__EntryPredicateArgsAsPrologSrc),
	listStrJoinComma(List__EntryPredicateArgsAsPrologSrc, Str__srcProlog__predicateArgs),
    
    
    count(List__EntryPredicateArgs,   Int__countOfArguments),
    ( Int__countOfArguments > 0 -> Str__srcProlog__predicateArgsComma = ',' ; Str__srcProlog__predicateArgsComma = '' ),
    





    
    

    
    strConcat([Str__SrcProlog__invoked],   Str__predicatesA),
    
    
    
    
    strFormat(" % collapse takes a expression in MeTTa which is nondeterministic and converts it deterministic controlflow and dataflow.\n %\n % we are using the predicate findall/3 to do this in Prolog.\n", [], Str__srcProlog__explaination),
    strFormat(" findall(FindallRes, pred~w(runtimeCtx(), runtimeCtx()  ~w~w, [~w], FindallRes),   ResCollection),\n", [Int__predicateId__invoked, Str__srcProlog__predicateArgsComma, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables],   Str__srcProlog__invokeFindall),
    strFormat(" Res = mettaExpr(ResCollection).\n", [],   Str__srcProlog__2),

    strConcat([Str__srcProlog__predicateHead, Str__srcProlog__explaination, Str__srcProlog__invokeFindall, Str__srcProlog__2],  Str__srcProlog__predicate),
    
    
    
    % emit complete code
    strConcat([Str__predicatesA, Str__srcProlog__predicate],   Str__SrcProlog__dest),
    
    write("compile trace: EXIT invokeCollapse\n"),
    
    PredIdCounterOut is PredIdCounter2,
    
    true.





% emission of prolog code for match
% ex of MeTTa for which this generates code: (match self2 (datA $A) (+ 5 $A))
%
% /inparam Expr__query                     expression of the query as AST
% /inparam Str__srcProlog__predicateHead   Prolog-Code of head of predicate
% /inparam Int__idOfInvokedPred            id of the invoked predicate
genCodeV2_match(List__EntryPredicateArgs, Set__Str__letVariableNames,  Expr__query, Str__srcProlog__predicateHead, Int__idOfInvokedPred) -->
	{
		% predicate is already allocated with
		%%allocatePredicate(PredIdCounterIn, PredIdCounterOut, List__EntryPredicateArgs, Set__Str__letVariableNames,     Int__PredicateIdRes, Str__srcProlog__predicateHead),
		
		% convert args from "List__EntryPredicateArgs" to string
		convListOfArgumentsToPrologSrc(List__EntryPredicateArgs,   Str__srcProlog__entryPredicateArgs),
        %term_string(Str__srcProlog__entryPredicateArgs2, Str__srcProlog__entryPredicateArgs), % HACK : make sure that it is a string


		% build the string of let variables
		convLetVariablesToPrologSrc(Set__Str__letVariableNames,   Str__srcProlog__letVariablesAsList),

        %%%Str__srcProlog__head = "pred43(runtimeCtx(), runtimeCtx(), TODOargs, TODOLetVarsStr,   Res) :-",


    	% we have to emit the leading Prolog predicate head
    	%%%%strFormat("pred~w(runtimeCtx(), runtimeCtx(), ~w, TODOLetVarsStr,   Res) :-~n", [PredIdCounterIn, Str__SrcProlog__Args],   Str__srcProlog__head),

        % the term of the expression to evaluate by MeTTa match()   : Str__srcProlog__expressionToQuery
		%
		% "Expr__query" is the expression of the query (as from the AST)
		%%%Expr__query = decoratedMettaExpr(nil, []), % for testing
		convAstExprToTargetProlog(Expr__query, "VarLet__",   Str__srcProlog__expressionToQuery),


		convIntToStr(Int__idOfInvokedPred,   Str__idOfInvokedPred),

        write(Str__srcProlog__entryPredicateArgs),nl, % DEBUG

        true
    },
    
    Str__srcProlog__predicateHead,
	" % build the expression to query\n",
	" Expr__Query = ", Str__srcProlog__expressionToQuery, ",\n",
	" \n",
	" datSpace('self2', Expr__Query), % query the space\n",
	" \n",
	" % invoke predicate to evaluate expression\n",
	" pred",Str__idOfInvokedPred,"(runtimeCtx(), runtimeCtx() ",Str__srcProlog__entryPredicateArgs," ,",Str__srcProlog__letVariablesAsList,",   Res),\n",
	" true.\n".


% query for manual testing is
% 
%List__EntryPredicateArgs=["a"], Set__Str__letVariableNames=["X"], Str__srcProlog__predicateHead="PREDICATEHEAD", 
%Expr__query=decoratedMettaExpr(nil, []), Int__idOfInvokedPred=53, 
%phrase(genCodeV2_match(List__EntryPredicateArgs, Set__Str__letVariableNames,  Expr__query, Str__srcProlog__predicateHead, Int__idOfInvokedPred), Chars), !, string_chars(Str, Chars).
%
%
%phrase(genCodeV2_match(["a"], ["X"],  decoratedMettaExpr(nil, []), "PREDICATEHEAD", "53"), Chars).



emitPrologFunctionForAst__Recursive(decoratedMettaExpr(invokeFunction(_),['match', 'self2', Expr__query, AstNode__body]), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, Int__PredicateIdRes) :-

    !, % drop all other backtracking

    logCompileTrace("matchA", "ENTER codegen for match"),


    % allocate predicate
    allocatePredicate(PredIdCounterIn, PredIdCounter2, List__EntryPredicateArgs, Set__Str__letVariableNames,     Int__PredicateIdRes, Str__srcProlog__predicateHead),

    % generate code for "AstNode__called"
    emitPrologFunctionForAst__Recursive(AstNode__body, ctx(PredIdCounter2), ctx(PredIdCounter3), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__body, Int__predicateId__body),


    %testing
    %Set__Str__letVariableNames=["X"],
    %Expr__query=decoratedMettaExpr(nil, []),
    %Int__predicateId__body=53, 
    %
    phrase(genCodeV2_match(List__EntryPredicateArgs, Set__Str__letVariableNames,  Expr__query, Str__srcProlog__predicateHead, Int__predicateId__body), Chars), !, string_chars(Str__srcProlog__generatedPredicate, Chars),

    %%%%%Str__srcProlog__generatedPredicate


    % DBG
    write(Str__srcProlog__generatedPredicate),nl,

    % concat all generated code
    strConcat([Str__SrcProlog__body, Str__srcProlog__generatedPredicate],   Str__SrcProlog__dest),


    PredIdCounterOut = PredIdCounter3,

    logCompileTrace("matchA", "EXIT SUCCESS codegen for match"),

    true.

% manual test with
% ?- Expr__query=decoratedMettaExpr(nil,[]), AstNode__called=nil  , emitPrologFunctionForAst__Recursive__(decoratedMettaExpr(todo,['match', 'self2', Expr__query, AstNode__called]), ctx(0), ctx(PredIdCounterOut), ["argA"], ["varA"], [],    Str__SrcProlog__dest, Int__PredicateIdRes).








extractCallsitesAndPredicateCodeFromListOfPredicateSiteInfo([],   [], []).
extractCallsitesAndPredicateCodeFromListOfPredicateSiteInfo([tuplePredicateSiteInfo(Str__srcProlog__predicate, Str__srcProlog__invocationSite, Int__PredicateIdRes)|List__tail],    [Str__srcProlog__invocationSite|List__tail__str__srcProlog__invocationsites], [Str__srcProlog__predicate|List__tail__str__predicatesOfArgs]) :-
    extractCallsitesAndPredicateCodeFromListOfPredicateSiteInfo(List__tail,   List__tail__str__srcProlog__invocationsites, List__tail__str__predicatesOfArgs).







% AST-node to invoke a metta function
emitPrologFunctionForAst__Recursive(Val2, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, PredIdCounterIn) :-

    logCompileTrace("FNGEN", "emission of code for function invocation/expression building"),

    decoratedMettaExpr(invokeFunction(Str__nameOfFunction),[_|Arr__astNodesOfArguments]) = Val2,

    % build the string of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string


    allocatePredicate(PredIdCounterIn, PredIdCounter1, List__EntryPredicateArgs, Set__Str__letVariableNames,  Int__PredicateIdRes, Str__srcProlog__predicateHead), % allocate new predicate, generate head of predicate
    
    
    
    

    %helperForAst__invokeFunction(Arr__astNodesOfArguments,   Str__srcProlog__predicateArgs), % generate name of variables which are unique in this predicate. note that we only care about the number of elements in List Arr__astNodesOfArguments
    
    
    % generate name of variables which are unique in this predicate
    count(Arr__astNodesOfArguments,   Int__n), % count how many elements are in list Arr__astNodesOfArguments
    helperForAst__genArgumentsForInvocationOfPredicate('ResultFromPredicate', List__varnamesOfArguments,  Int__n),
    listStrJoinComma(List__varnamesOfArguments, Str__srcProlog__predicateArgs), % join list of variable names to string
    


    % * generate prolog code of invoked functions
    genPredicateInvocations(Arr__astNodesOfArguments, List__varnamesOfArguments,  ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,       List__tuple__predicateSiteInfo),
    extractCallsitesAndPredicateCodeFromListOfPredicateSiteInfo(List__tuple__predicateSiteInfo,    List__str__srcProlog__callsites, List__str__predicatesOfArgs), % extract predicates and callsite from list

    strConcat(List__str__srcProlog__callsites, Str__srcProlog__callsites),
    strConcat(List__str__predicatesOfArgs, Str__srcProlog__predicatesOfArgs),
    


    set_contains(List__Str__functionNames, Str__nameOfFunction,   Bool__isFunctionInvocation),
    

    ( Bool__isFunctionInvocation
    ->
        % ... generate predicate call for MeTTa function invocation
        %strFormat(' pred__(runtimeCtx(), runtimeCtx(), mettaExpr([\'~w\',~w]), [~w], Res), % invoke predicate of AST-node invokeFunction\n', [Str__nameOfFunction, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables], Str__srcProlog__invokePredicate)   % generate code to invoke the predicate
        strFormat(' pred__(runtimeCtx(), runtimeCtx(), mettaExpr([\'~w\',~w]),   Res), % invoke predicate of AST-node invokeFunction\n', [Str__nameOfFunction, Str__srcProlog__predicateArgs], Str__srcProlog__invokePredicate)   % generate code to invoke the predicate
    ;   
        % ... generate construction of result Expr
        strFormat(' Res = mettaExpr([~w,~w]), % build result metta-expression\n', [Str__nameOfFunction, Str__srcProlog__predicateArgs], Str__srcProlog__invokePredicate)
    ),
    



    Str__srcProlog__predicateTail = ' true.\n',
    
    strConcat([Str__srcProlog__predicatesOfArgs,  Str__srcProlog__predicateHead, Str__srcProlog__callsites, Str__srcProlog__invokePredicate, Str__srcProlog__predicateTail],   Str__SrcProlog__dest),
    

    logCompileTrace("FNGEN", "done"),
    
    PredIdCounterOut is PredIdCounter2, % assign counter to output variable
    
    true.

% manual-test
%   ?- emitPrologFunctionForAst__Recursive(astNode(invokeFunction, 'calledA', [ astNode(assignConstInt, 2)  ]), ctx(0), ctx(PredIdCounterOut), ['entryPredArg0'], Str__SrcProlog__dest, Int__PredicateIdRes).



% generate code for invocations of a list of AstNodes to be stored into target variables
% List__varnamesOfArguments,  Arr__astNodesOfArguments
genPredicateInvocations([], [], ctx(PredIdCounter), ctx(PredIdCounter), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,       []).
genPredicateInvocations([AstNode__body|List__tail__astNodes], [Str__varnameOfResult|List__str__varnameOfResult], ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,         [Tuple|List__tail__]) :-
    
    PredIdCounter0 is PredIdCounterIn,
   
    genPredicateInvocations(List__tail__astNodes, List__str__varnameOfResult, ctx(PredIdCounter0), ctx(PredIdCounter1), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,        List__tail__),
    
    % generate invocation and body for head
    genPredicateInvocation(AstNode__body, Str__varnameOfResult,  ctx(PredIdCounter1), ctx(PredIdCounter2), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,        Tuple),

    PredIdCounterOut is PredIdCounter2,
    
    true.


% manual-test
%    ?- genPredicateInvocations([astNode(assignConstInt, 2)], ['VRes0'],  ctx(0), ctx(PredIdCounterOut), ['a', 'b'],   List__tuple__predicateSiteInfo).



% generate code for call to a predicate and of the body of the predicate itself
genPredicateInvocation(AstNode__body, Str__varnameOfResult,  ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,      Tuple) :-

    % build the string of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string


    % generate prolog code for body of AST-node
    emitPrologFunctionForAst__Recursive(AstNode__body, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,       Str__srcProlog__predicate, Int__PredicateIdRes2),


    % convert List__EntryPredicateArgs to strings Str__srcProlog__predicateArgsComma and Str__srcProlog__predicateArgs
    convListOfVariablesAndPrefixToStringAndComma(List__EntryPredicateArgs, 'VArg__',    Str__srcProlog__predicateArgs, Str__srcProlog__predicateArgsComma),

    %%%print(Str__srcProlog__predicateArgs), % DBG


    % generate prolog code for invocation-site
    strFormat(' pred~w(runtimeCtx(), runtimeCtx() ~w~w, [~w],   ~w), % invoke predicate which implements body of argument\n', [Int__PredicateIdRes2, Str__srcProlog__predicateArgsComma, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables, Str__varnameOfResult],   Str__srcProlog__invocationSite),

    Tuple = tuplePredicateSiteInfo(Str__srcProlog__predicate, Str__srcProlog__invocationSite, Int__PredicateIdRes2),

    true.


% manual-test
%    ?- genPredicateInvocation(astNode(assignConstInt, 2), 'VRes0',  ctx(0), ctx(PredIdCounterOut), ['a', 'b'],   Tuple).









% conditional
emitPrologFunctionForAst__Recursive(decoratedMettaExpr(cond,['if', AstNode__Cond, AstNode__TrueCodepath, AstNode__FalseCodepath]), ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, Int__PredicateIdRes) :-

    % FIXME MID : rewrite generated prolog code to use built in condition support of prolog


    % build the string and stuff of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string


	allocatePredicate(PredIdCounterIn, PredIdCounter1, List__EntryPredicateArgs, Set__Str__letVariableNames,   Int__PredicateIdRes, Str__srcProlog__predicateHead), % allocate new predicate, generate head of predicate
    
    
    % allocate a predicate for the helper
    allocatePredicate(PredIdCounter1, PredIdCounter2, _, Set__Str__letVariableNames,   Int__predicateId__predicateOfHelper, _), % allocate new predicate, generate head of predicate
    
    
    
    emitPrologFunctionForAst__Recursive(AstNode__Cond, ctx(PredIdCounter2), ctx(PredIdCounter3), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__condition, Int__predicateId__condition),
    
    emitPrologFunctionForAst__Recursive(AstNode__TrueCodepath, ctx(PredIdCounter3), ctx(PredIdCounter4), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__trueCodepath, Int__predicateId__trueCodepath),
    
    emitPrologFunctionForAst__Recursive(AstNode__FalseCodepath, ctx(PredIdCounter4), ctx(PredIdCounter5), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,     Str__SrcProlog__falseCodepath, Int__predicateId__falseCodepath),
    
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
    
    
    strFormat('pred~w(true, runtimeCtx(), runtimeCtx(), ~w, [~w], Res) :-\n', [Int__predicateId__predicateOfHelper, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables], Str__srcProlog__helperTrueCodepath),
    
    strFormat(' pred~w(runtimeCtx(), runtimeCtx(), ~w, [~w], Res), % invoke predicate of codepath of true\n true.\n', [Int__predicateId__trueCodepath, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables], Str__srcProlog__predicatebodyForTrueCodepath),
	
    
    strFormat('pred~w(false, runtimeCtx(), runtimeCtx(), ~w, [~w], Res) :-\n', [Int__predicateId__predicateOfHelper, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables], Str__srcProlog__helperFalseCodepath),
	
    strFormat(' pred~w(runtimeCtx(), runtimeCtx(), ~w, [~w], Res), % invoke predicate of codepath of false\n true.\n', [Int__predicateId__falseCodepath, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables], Str__srcProlog__predicatebodyForFalseCodepath),
    
    
    
    strFormat('\n\n\n~w~w\n\n~w~w', [Str__srcProlog__helperTrueCodepath, Str__srcProlog__predicatebodyForTrueCodepath, Str__srcProlog__helperFalseCodepath, Str__srcProlog__predicatebodyForFalseCodepath],   Str__srcProlog__predicatesForConditionIndirection),
    
    
    
    strConcat([Str__srcProlog__calledPredicates, Str__srcProlog__predicatesForConditionIndirection],   Str__predicatesA),
    
    
    
    % now we generate code for the "real" predicate. This predicate calls into the generated helper predicate after computing the value of the AstNode__Cond AST-node
    
    strFormat(' pred~w(runtimeCtx(), runtimeCtx(), ~w, [~w], CondEvalResult), % evaluate value of condition\n', [Int__predicateId__condition, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables],   Str__srcProlog__evalValueOfCondition),
    strFormat(' pred~w(CondEvalResult, runtimeCtx(), runtimeCtx(), ~w, [~w], Res),\n', [Int__predicateId__predicateOfHelper, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables],   Str__srcProlog__invokeConditionalHelperPredicate),
    strConcat([Str__srcProlog__evalValueOfCondition, Str__srcProlog__invokeConditionalHelperPredicate, ' true.\n'],   Str__srcProlog__predicateBody),
    
    
    strConcat([Str__srcProlog__predicateHead, Str__srcProlog__predicateBody],   Str__srcProlog__predicate),
    
    
    % emit complete code
    strConcat([Str__predicatesA, Str__srcProlog__predicate],   Str__SrcProlog__dest),
    
    
    PredIdCounterOut is PredIdCounter5,
    
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



% /param AstNode__headArgument   is the AST-node of the head of the MeTTa function definition as a decoratedMettaExpr(<DECORATION>, <CONTENT>)
emitPrologFunctionOfMettaFunctionDefinition(
    mettaFunctionDefinition(AstNode__headArgument, AstNode__body__in),
    ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__Str__functionNames,    Str__SrcProlog__dest, Int__PredicateIdRes
) :-


    % we collect the variable names from the head of the function-definition from MeTTa
    ast__collectVarNames(AstNode__headArgument, [],   Set__Str__headVariables),

    %format("\nDBG  Set__Str__headVariables=\n"),
    %format("~w\n", [Set__Str__headVariables]),


    format('DBG  AstNode__body__in=\n'),
    format('~w\n', [AstNode__body__in]),

    % decorate AST-nodes with the statically known controlflow type of nodes
    codeanalysis__decorateWithStatic(AstNode__body__in,   AstNode__decoratedBody),
    !, % we only care about one tree!

    format('DBG  AstNode__decoratedBody=\n'),
    format('~w\n', [AstNode__decoratedBody]),

    % convert the head-argument to the Prolog source, which represents the head. Idea here is to leave the heavy lifting for unification to the prolog runtime of the prolog target
    convAstExprToTargetProlog(AstNode__headArgument, 'Varg__',    Str__srcProlog__headArgument),

    %format('DBG ~w\n', Str__srcProlog__headArgument),




    Set__Str__letVariableNames__0 = [], % empty set
    codeanalysis__collectLetVariables(AstNode__decoratedBody, Set__Str__letVariableNames__0,   Set__Str__letVariableNames__1), % analyze body for let variables and collect let variables
    !, % cut because we don't care about multiple backtracking results of let variables

    format('DBG  MeTTa fn def:   letVariableNames=\n'),
    format('~w\n', [Set__Str__letVariableNames__1]),

    logCompileTrace("MeTTa fn def", "call into emission of code for body..."),

	emitPrologFunctionForAst__Recursive(AstNode__decoratedBody, ctx(PredIdCounterIn), ctx(PredIdCounter1), Set__Str__headVariables, Set__Str__letVariableNames__1, List__Str__functionNames,   Str__SrcProlog__ofPredicates, Int__PredicateIdCalled),

    logCompileTrace("MeTTa fn def", "...done"),
    

	
    % build the string of the arguments
    zipListStrWithPrefix(Set__Str__headVariables, 'Varg__',   Set__Str__headVariablesWithPrefix),
    listStrJoinComma(Set__Str__headVariablesWithPrefix, Str__srcProlog__predicateArgs), % join list of variable names to string
    

    count(Set__Str__headVariablesWithPrefix,   Int__countOfArguments),
    ( Int__countOfArguments > 0 -> Str__srcProlog__predicateArgsComma = ',' ; Str__srcProlog__predicateArgsComma = '' ),
    


    % build the string and stuff of let variables
    zipListStrWithPrefix(Set__Str__letVariableNames__1, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix, Str__srcProlog__letVariables), % join list of variable names to string
    



    
    %strFormat('pred__(runtimeCtx(), runtimeCtx(), ~w, [~w],   Res) :-\n', [Str__srcProlog__headArgument, Str__srcProlog__letVariables],   Str__srcProlog__head),
    strFormat('pred__(runtimeCtx(), runtimeCtx(), ~w,   Res) :-\n', [Str__srcProlog__headArgument],   Str__srcProlog__head),
    
    strFormat(' pred~w(runtimeCtx(), runtimeCtx() ~w~w, [~w],    Res), % invoke predicate which implements body of function written in metta\n', [Int__PredicateIdCalled, Str__srcProlog__predicateArgsComma, Str__srcProlog__predicateArgs, Str__srcProlog__letVariables],   Str__srcProlog__invokeBodyPredicate),
    
    Str__srcProlog__predicateEnd = ' true.\n',
    
    strConcat([Str__SrcProlog__ofPredicates, '\n', Str__srcProlog__head, Str__srcProlog__invokeBodyPredicate, Str__srcProlog__predicateEnd], Str__SrcProlog__dest),
    
	PredIdCounterOut = PredIdCounter1,
	
	true.





emitPrologFunctionOfMettaFunctionDefinitions(
    [],
    ctx(PredIdCounter), ctx(PredIdCounter), _,    ""
) :-
    true.

emitPrologFunctionOfMettaFunctionDefinitions(
    [mettaFunctionDefinition(AstNode__headArgument, AstNode__body__in)|List__tail],
    ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__Str__functionNames,    Str__SrcProlog__dest
) :-

    emitPrologFunctionOfMettaFunctionDefinitions(
        List__tail,
        ctx(PredIdCounterIn), ctx(PredIdCounter1), List__Str__functionNames,    Str__SrcProlog__fromRecursion
    ),

    emitPrologFunctionOfMettaFunctionDefinition(
        mettaFunctionDefinition(AstNode__headArgument, AstNode__body__in),
        ctx(PredIdCounter1), ctx(PredIdCounterOut), List__Str__functionNames,    Str__SrcProlog__ofDefinition, _
    ),

    strConcat([Str__SrcProlog__ofDefinition, Str__SrcProlog__fromRecursion],    Str__SrcProlog__dest),

    true.















%%%%%%%%%%%%%%%%%%%%%%
% AST analysis



codeanalysis__decorateWithStatic__helperForList([],   []).
codeanalysis__decorateWithStatic__helperForList([Head__in|Tail__in],   [Head__out|Tail__out]) :-
    codeanalysis__decorateWithStatic__helperForList(Tail__in,   Tail__out),
    codeanalysis__decorateWithStatic(Head__in,   Head__out).


% codeanalysis__decorateWithStatic() decorates AST-nodes with the statically known controlflow type
% does NOT check for semantic correctness!

codeanalysis__decorateWithStatic(Number,   Number) :-
    number(Number).
codeanalysis__decorateWithStatic(Bool,   Bool) :-
    checkBoolean(Bool).
codeanalysis__decorateWithStatic(Str,   Str) :-
    atom(Str), % Str must be a string.

    true.
codeanalysis__decorateWithStatic(var(Name),   var(Name)).

codeanalysis__decorateWithStatic(decoratedMettaExpr(nil, ['if', AstNode__in__cond, AstNode__in__TrueCodepath, AstNode__in__FalseCodepath]),   decoratedMettaExpr(cond, ['if', AstNode__out__cond, AstNode__out__TrueCodepath, AstNode__out__FalseCodepath])) :-
    codeanalysis__decorateWithStatic(AstNode__in__cond,   AstNode__out__cond),
    codeanalysis__decorateWithStatic(AstNode__in__TrueCodepath,   AstNode__out__TrueCodepath),
    codeanalysis__decorateWithStatic(AstNode__in__FalseCodepath,   AstNode__out__FalseCodepath).


codeanalysis__decorateWithStatic(
    decoratedMettaExpr(nil, ['let2',  AstNode__varAssignments__in, AstNode__body__in]),
    decoratedMettaExpr(let2, ['let2',  AstNode__varAssignments__out, AstNode__body__out])
) :-
    codeanalysis__decorateWithStatic(AstNode__varAssignments__in,   AstNode__varAssignments__out),
    codeanalysis__decorateWithStatic(AstNode__body__in,   AstNode__body__out),
    true.


codeanalysis__decorateWithStatic(decoratedMettaExpr(nil, [Str__functionname|List__AstNode__in__arguments]),   decoratedMettaExpr(invokeFunction(Str__functionname), [Str__functionname|List__AstNode__out__arguments])) :-
    
    atom(Str__functionname), % must be string
    % TODO : refactor code so we are really using strings instead of atoms!

    codeanalysis__decorateWithStatic__helperForList(List__AstNode__in__arguments,   List__AstNode__out__arguments).



% else we fallback to simple recusion without decorating anything
codeanalysis__decorateWithStatic(decoratedMettaExpr(nil, List__in),   decoratedMettaExpr(nil, List__out)) :-
    codeanalysis__decorateWithStatic__helperForList(List__in,   List__out).








% helper to collect variable names of the actual assignments of a let expression
codeanalysis__collectLetVariables__helperForAssignments([], Set__Str__letVariableNames,   Set__Str__letVariableNames).
codeanalysis__collectLetVariables__helperForAssignments([decoratedMettaExpr(_,[var(Str__varname)|_])|List__tail__in], Set__Str__letVariableNames__in,   Set__Str__letVariableNames__out) :-
    codeanalysis__collectLetVariables__helperForAssignments(List__tail__in, Set__Str__letVariableNames__in,   Set__Str__letVariableNames__1),
    set_put(Str__varname, Set__Str__letVariableNames__1,   Set__Str__letVariableNames__out),
    true.


codeanalysis__collectLetVariables__helperForTree([], Set__Str__letVariableNames,   Set__Str__letVariableNames).
codeanalysis__collectLetVariables__helperForTree([Head|Tail], Set__Str__letVariableNames__in,   Set__Str__letVariableNames__out) :-
    codeanalysis__collectLetVariables__helperForTree(Tail, Set__Str__letVariableNames__in,   Set__Str__letVariableNames__0),
    codeanalysis__collectLetVariables(Head, Set__Str__letVariableNames__0,   Set__Str__letVariableNames__out).


% recursivly collect variable names of let2 assignment variables for this and all tree-subitems

codeanalysis__collectLetVariables(decoratedMettaExpr(let2,['let2',decoratedMettaExpr(nil,List__AstNode__assignments),AstNode__child]), Set__Str__letVariableNames__in,   Set__Str__letVariableNames__out) :-
    Set__Str__letVariableNames__0 = Set__Str__letVariableNames__in,

    codeanalysis__collectLetVariables(AstNode__child, Set__Str__letVariableNames__0,   Set__Str__letVariableNames__1),
    codeanalysis__collectLetVariables__helperForAssignments(List__AstNode__assignments, Set__Str__letVariableNames__1,   Set__Str__letVariableNames__2),

    Set__Str__letVariableNames__out = Set__Str__letVariableNames__2,
    true.

codeanalysis__collectLetVariables(decoratedMettaExpr(_,List__AstNode__in), Set__Str__letVariableNames__in,   Set__Str__letVariableNames__out) :-
    codeanalysis__collectLetVariables__helperForTree(List__AstNode__in, Set__Str__letVariableNames__in,   Set__Str__letVariableNames__out).

% case when it is not a tree. In this case we don't have any new let assingment variables to add to the set
codeanalysis__collectLetVariables(_, Set__Str__letVariableNames,   Set__Str__letVariableNames).









%  emits mettaExpr(<LIST>)
% helper to convert list of AST-expression to list of strings of prolog target code
convAstExprToTargetProlog__helper__convList([], _,   []).
%%convAstExprToTargetProlog__helper__convList([AstExpr], [Str__srcProlog]) :-
%%    convAstExprToTargetProlog(AstExpr,   Str__srcProlog).
convAstExprToTargetProlog__helper__convList([AstExpr|List__astTail], Str__variablePrefix,   [Str__srcProlog|List__str__srcProlog]) :-
    convAstExprToTargetProlog__helper__convList(List__astTail, Str__variablePrefix,   List__str__srcProlog),
    convAstExprToTargetProlog(AstExpr, Str__variablePrefix,   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(decoratedMettaExpr(_, List__inner), Str__variablePrefix,   Str__srcProlog) :-
    
    convAstExprToTargetProlog__helper__convList(List__inner, Str__variablePrefix,   List__str__srcPrologInner),
    listStrJoin(List__str__srcPrologInner, ',',   Str__strProlog__inner),
    
    strFormat('mettaExpr([~w])', [Str__strProlog__inner],   Str__srcProlog),
    
    !,
    
    true.


% convert a AST-expression to prolog target
% Str__variablePrefix is the prefix for used variable-names
convAstExprToTargetProlog(var(Str__name), Str__variablePrefix,   Str__srcProlog) :-
    strFormat('~w~w', [Str__variablePrefix, Str__name],   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(Val, _,    Str__srcProlog) :-
    number(Val), % Val must be a number
    strFormat('~w', [Val],   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(Val, _,    Str__srcProlog) :-
    checkBoolean(Val), % Val must be a boolean
    strFormat('~w', [Val],   Str__srcProlog).

% convert a AST-expression to prolog target
convAstExprToTargetProlog(Str, _,    Str__srcProlog) :-
    checkIsString(Str), % Val must be a string
    strFormat('\'~w\'', [Str],   Str__srcProlog).


% manual test
%    ?- convAstExprToTargetProlog(decoratedMettaExpr(nil, ['a', 5, var('B')]), 'Varg__',    Str).








ast__collectVarNames__helperForList([], Set,   Set).
ast__collectVarNames__helperForList([Head|Tail], Set__in,   Set__out) :-
    ast__collectVarNames__helperForList(Tail, Set__in,   Set__0),
    ast__collectVarNames(Head, Set__0,   Set__out).

% collect names of variables
ast__collectVarNames(decoratedMettaExpr(_, List__AstNode__children), Set__in,   Set__out) :-
    ast__collectVarNames__helperForList(List__AstNode__children, Set__in,   Set__out).

ast__collectVarNames(var(Str__name), Set__in,   Set__out) :-
    set_put(Str__name, Set__in,   Set__out).

ast__collectVarNames(_, Set,   Set). % default fallback for all other stuff in the tree






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






















% extract functionname.
functionDefinition__extractFunctionname(mettaFunctionDefinition(decoratedMettaExpr(_, [Str__functionname|_]), _),   Str__functionname) :-
    atom(Str__functionname). % must be a atom

functionDefinitions__extractFunctionnames([],   []).
functionDefinitions__extractFunctionnames([H|T],   [Str_head|List__Str__tail]) :-
    functionDefinitions__extractFunctionnames(T,   List__Str__tail),
    functionDefinition__extractFunctionname(H,   Str_head).




% used for debugging problems with AST codegen
%emitPrologFunctionForAst__Recursive(Val, ctx(PredIdCounterIn), ctx(PredIdCounterOut), List__EntryPredicateArgs, Set__Str__letVariableNames, List__Str__functionNames,    Str__SrcProlog__dest, PredIdCounterIn) :-
%    write("compile trace: FAIL\n"),
%
%    print(Val),nl,
%
%    false.






















% helper
convIntToStr(Int,   Str) :-
	strFormat("~w", [Int],   Str).


% convert list of strings of arguments to Prolog-Source code
%
convListOfArgumentsToPrologSrc([],   ",").
convListOfArgumentsToPrologSrc(List__str__list,   Str__concatenated) :-
	listStrJoin(List__str__list, ",",   Str__concatenated2),
    strConcat([",", Str__concatenated2],   Str__concatenated). % add comma in front




% build the string of let variables
convLetVariablesToPrologSrc(Set__Str__letVariableNames,   Str__srcProlog__letVariablesAsList) :-
    zipListStrWithPrefix(Set__Str__letVariableNames, 'VLet__',   Set__Str__letVariablesWithPrefix),
    listStrJoinComma(Set__Str__letVariablesWithPrefix,   Str__srcProlog__letVariables), % join list of variable names to string

    strFormat("[~w]", [Str__srcProlog__letVariables],   Str__srcProlog__letVariablesAsList).








