% MeTTa to prolog parser


% pack_install(tokenize).
:- use_module(library(tokenize)).


% see https://stackoverflow.com/questions/7808766/how-to-check-a-variable-is-a-string-or-a-number-in-prolog
%checkIsString(Object) :-
%    forall(member(X, Object), number(X)).



%
% decoratedMettaExpr(<DECORATION>, <LIST OF CONTENT>)



expr(literal('+')) --> [punct(+)]. % is also a literal in metta
expr(literal('-')) --> [punct(-)]. % is also a literal in metta
expr(literal('*')) --> [punct(*)]. % is also a literal in metta
expr(literal('/')) --> [punct(/)]. % is also a literal in metta

expr(literal(Str)) --> [word(Str)].%, {checkIsString(Str)}.
expr(boolean(true)) --> [word('true')].
expr(boolean(false)) --> [word('false')].
%expr(number_(Val)) --> [Str], {atom_number(Str, Val)}.
expr(number_(Val)) --> [number(Val)].

expr(var(Name)) --> [punct($), word(Name)].


expr(X) --> brace2(X).

braceContent(braceContent()) --> [punct(')')].
braceContent(braceContent(X,Y)) --> expr(X), braceContent(Y).


brace2(parsedBrace(X)) --> [punct('(')],  braceContent(X).


braceOrExpression(X) --> brace2(X).
braceOrExpression(X) --> expr(X).



functionDeclaration(mettaFunctionDefinition(Head, Body)) -->
    [punct('('), punct('=')], 
    brace2(Head),
    braceOrExpression(Body),
    [punct(')')].





% invocation(invocation(Brace)) --> [atomTODO('!')], brace2(Brace).


% rootList([Invocation|Tail]) --> invocation(Invocation), rootList(Tail).
rootList([FnDef|Tail]) --> functionDeclaration(FnDef), rootList(Tail).
rootList([]) --> [].






% manual test with
% phrase(expr(Tree), [punct('('), word('TODO'), punct(')')]).



% helper to fold braces of the tree
foldTreeBrace(braceContent(),   []).
foldTreeBrace(braceContent(H,T),   [FoldedHead|List__tail]) :-
    foldTree(H,   FoldedHead),
    foldTreeBrace(T,   List__tail).

foldTree(parsedBrace(X),   parsedBrace2(FoldedContent)) :-
    foldTreeBrace(X,  FoldedContent).

foldTree(mettaFunctionDefinition(ParseTree__InputHead, ParseTree__InputBody),   mettaFunctionDefinition(ParseTree__ResultHead, ParseTree__ResultBody)) :-
    foldTree(ParseTree__InputHead,   ParseTree__ResultHead),
    foldTree(ParseTree__InputBody,   ParseTree__ResultBody).

foldTree(X,   X).





% helper to fold all items of the root-list
foldRootList([],   []).
foldRootList([mettaFunctionDefinition(ParseTree__in__head, ParseTree__in__body)|Tail__in],   [mettaFunctionDefinition(ParseTree__out__head, ParseTree__out__body)|Tail__out]) :-
    foldRootList(Tail__in,   Tail__out),
    foldTree(mettaFunctionDefinition(ParseTree__in__head, ParseTree__in__body),   mettaFunctionDefinition(ParseTree__out__head, ParseTree__out__body)).

% manual-test with
% ?- X = [mettaFunctionDefinition(parsedBrace(braceContent()),parsedBrace(braceContent()))], foldRootList(X, Y).



% remove space(' ') token and remove cntrl('\n') token
tokens__removeSpace([],   []).
tokens__removeSpace([space(' ')|List__tokens__tail],   List__tail) :-
    tokens__removeSpace(List__tokens__tail,   List__tail).
tokens__removeSpace([cntrl('\n')|List__tokens__tail],   List__tail) :-
    tokens__removeSpace(List__tokens__tail,   List__tail).

tokens__removeSpace([Token__head|List__tokens__tail],   [Token__head|List__tail]) :-
    tokens__removeSpace(List__tokens__tail,   List__tail).



% tokenize(`(+ 5 7)`, Tokens), tokens__removeSpace(Tokens,   Tokens2), !.
%

%?- tokenize(`(add2 5 7)`, Tokens), tokens__removeSpace(Tokens,   Tokens2), !.
%Tokens = [punct('('), word(add2), space(' '), number(5), space(' '), number(7), punct(')')],
%Tokens2 = [punct('('), word(add2), number(5), number(7), punct(')')].






convParseTreeToAst__braceHelper([],   []).
convParseTreeToAst__braceHelper([H|T],   [Ast__head|List__ast__tail]) :-
    convParseTreeToAst(H,   Ast__head),
    convParseTreeToAst__braceHelper(T,   List__ast__tail).


% convert parsing tree to AST
convParseTreeToAst(parsedBrace2([]),   decoratedMettaExpr(nil, [])).
%%%convParseTreeToAst(parsedBrace2([literal('if')|List]),   decoratedMettaExpr(cond, List__ast)) :-
%%%    convParseTreeToAst__braceHelper(List,  List__ast).
%%%convParseTreeToAst(parsedBrace2([literal(Atom)|List]),   decoratedMettaExpr(invokeFunction(Str), List__ast)) :-
%%%    atom_string(Atom, Str), % convert atom to string!
%%%
%%%    convParseTreeToAst__braceHelper(List,  List__ast).
convParseTreeToAst(parsedBrace2(List__parseTree),   decoratedMettaExpr(nil, List__ast)) :-
    convParseTreeToAst__braceHelper(List__parseTree,  List__ast).


/*
% HACKY to allow calling of built in algebra (for now)
convParseTreeToAst(parsedBrace2([literal('+')|List]),   astNode('+', List__ast)) :-
    convParseTreeToAst__braceHelper(List,  List__ast).
convParseTreeToAst(parsedBrace2([literal('-')|List]),   astNode('-', List__ast)) :-
    convParseTreeToAst__braceHelper(List,  List__ast).
convParseTreeToAst(parsedBrace2([literal('*')|List]),   astNode('*', List__ast)) :-
    convParseTreeToAst__braceHelper(List,  List__ast).
convParseTreeToAst(parsedBrace2([literal('/')|List]),   astNode('/', List__ast)) :-
    convParseTreeToAst__braceHelper(List,  List__ast).
 */

convParseTreeToAst(literal(Str),   Str).

convParseTreeToAst(number_(Int),   Int).

convParseTreeToAst(var(Name),   var(Name)).

convParseTreeToAst(mettaFunctionDefinition(ParseTree__Head, ParseTree__Body),   mettaFunctionDefinition(Ast__Head, Ast__Body)) :-
    convParseTreeToAst(ParseTree__Head,   Ast__Head),
    convParseTreeToAst(ParseTree__Body,   Ast__Body).







parserForMetta(Str__srcMetta,   RootList__out) :-

    Int__compilerVerbosity = 2,


    tokenize(Str__srcMetta, Tokens),

    % DBG
    ( Int__compilerVerbosity >= 2 -> (
        format("\n\ntokens:\n"),
        print(Tokens),nl
    ) ; true ),

    tokens__removeSpace(Tokens,   Tokens2), 
    !, % throw all other tokenization away

    % DBG
    ( Int__compilerVerbosity >= 2 -> (
        format("\n\ntokens:\n"),
        print(Tokens2),nl
    ) ; true ),


    %phrase(functionDeclaration(ParseTreeA), Tokens2),
    phrase(rootList(RootList), Tokens2),
    !,
    
    % DBG
    ( Int__compilerVerbosity >= 2 -> (
        format("\n\nparse root raw:\n"),
        print(RootList),nl
    ) ; true ),
    
    
    % we need to fold the parsing tree to get a more useful representation
    foldRootList(RootList,   RootList1),
    !,
    
    % DBG
    ( Int__compilerVerbosity >= 2 -> (
        format("\n\nroot list folded:\n"),
        print(RootList1)
    ) ; true ),
    


    convRootParseListToRootListOfAsts(RootList1,   RootList2),
    !,


    % DBG
    ( Int__compilerVerbosity >= 2 -> (
        format("\n\nlist of AST:\n"),
        print(RootList2),nl
    ) ; true ),


    RootList__out = RootList2,


    true.



convRootParseListToRootListOfAsts([],   []).
convRootParseListToRootListOfAsts([Head__in|Tail__in],   [Head__out|Tail__out]) :-
    convRootParseListToRootListOfAsts(Tail__in,   Tail__out),
    convParseTreeToAst(Head__in,   Head__out).



