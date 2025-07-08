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

expr(X) --> brace2(X).

braceContent(braceContent()) --> [punct(')')].
braceContent(braceContent(X,Y)) --> expr(X), braceContent(Y).


brace2(parsedBrace(X)) --> [punct('(')],  braceContent(X).




functionDeclaration(functionDeclaration(Head, Body)) -->
    [punct('('), punct('=')], 
    brace2(Head),
    brace2(Body),
    [punct(')')].





% manual test with
% phrase(expr(Tree), [punct('('), word('TODO'), punct(')')]).



% helper to fold braces
foldBrace(braceContent(),   []).
foldBrace(braceContent(H,T),   [FoldedHead|List__tail]) :-
    fold(H,   FoldedHead),
    foldBrace(T,   List__tail).

fold(parsedBrace(X),   parsedBrace2(FoldedContent)) :-
    foldBrace(X,  FoldedContent).

fold(functionDeclaration(ParseTree__InputHead, ParseTree__InputBody),   functionDeclaration(ParseTree__ResultHead, ParseTree__ResultBody)) :-
    fold(ParseTree__InputHead,   ParseTree__ResultHead),
    fold(ParseTree__InputBody,   ParseTree__ResultBody).

fold(X,   X).





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
convParseTreeToAst(parsedBrace2([literal('if')|List]),   decoratedMettaExpr(cond, List__ast)) :-
    convParseTreeToAst__braceHelper(List,  List__ast).
convParseTreeToAst(parsedBrace2([literal(Atom)|List]),   decoratedMettaExpr(invokeFunction(Str), List__ast)) :-
    atom_string(Atom, Str), % convert atom to string!

    convParseTreeToAst__braceHelper(List,  List__ast).

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

convParseTreeToAst(number_(Int),   Int).

convParseTreeToAst(functionDeclaration(ParseTree__Head, ParseTree__Body),   functionDeclaration(Ast__Head, Ast__Body)) :-
    convParseTreeToAst(ParseTree__Head,   Ast__Head),
    convParseTreeToAst(ParseTree__Body,   Ast__Body).







parserForMetta(Str__srcMetta,   Ast__result) :-

    tokenize(Str__srcMetta, Tokens),

    % DBG
    print(Tokens),
    nl,


    tokens__removeSpace(Tokens,   Tokens2), 
    !, % throw all other tokenization away

    % DBG
    nl,
    nl,
    print('tokens:'),
    nl,
    print(Tokens2),



    phrase(functionDeclaration(ParseTreeA), Tokens2),
    !,
    
    % DBG
    nl,
    nl,
    print('parse tree raw:'),
    nl,
    print(ParseTreeA),

    
    
    % we need to fold the parsing tree to get a more useful representation
    fold(ParseTreeA,   ParseTreeB),
    !,
    
    % DBG
    nl,
    nl,
    print('parse tree folded:'),
    nl,
    print(ParseTreeB),


    convParseTreeToAst(ParseTreeB,   Ast__result),
    !,


    % DBG
    nl,
    nl,
    print('AST:'),
    nl,
    print(Ast__result),



    true.
