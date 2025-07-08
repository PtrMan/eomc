% DRAFT of MeTTa to prolog parser

% run with
%    swipl -s ./src/compiler/parserMetta.prolog -g manualtest_parserA -g halt

% pack_install(tokenize).
:- use_module(library(tokenize)).


% see https://stackoverflow.com/questions/7808766/how-to-check-a-variable-is-a-string-or-a-number-in-prolog
%checkIsString(Object) :-
%    forall(member(X, Object), number(X)).



expr(literal(Str)) --> [word(Str)].%, {checkIsString(Str)}.
expr(boolean(true)) --> [word('true')].
expr(boolean(false)) --> [word('false')].
%expr(number_(Val)) --> [Str], {atom_number(Str, Val)}.
expr(number_(Val)) --> [number(Val)].

expr(X) --> brace2(X).

braceContent(braceContent()) --> [punct(')')].
braceContent(braceContent(X,Y)) --> expr(X), braceContent(Y).


brace2(parsedBrace(X)) --> [punct('(')],  braceContent(X).



% test with
% phrase(expr(Tree), [punct('('), word('TODO'), punct(')')]).



% helper to fold braces
foldBrace(braceContent(),   []).
foldBrace(braceContent(H,T),   [FoldedHead|List__tail]) :-
    fold(H,   FoldedHead),
    foldBrace(T,   List__tail).

fold(parsedBrace(X),   parsedBrace2(FoldedContent)) :-
    foldBrace(X,  FoldedContent).
fold(X,   X).





% remove space(' ') token
tokens__removeSpace([],   []).
tokens__removeSpace([space(' ')|List__tokens__tail],   List__tail) :-
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
convParseTreeToAst(parsedBrace2([literal(Atom)|List]),   astNode(invokeFunction, Str, List__ast)) :-
    atom_string(Atom, Str), % convert atom to string!
    
    convParseTreeToAst__braceHelper(List,  List__ast).
convParseTreeToAst(number_(Int),   astNode(assignConstInt, Int)).
convParseTreeToAst(X,   X).





parserForMetta(Str__srcMetta,   Ast__result) :-

    tokenize(Str__srcMetta, Tokens),

    % DBG
    print(Tokens),
    nl,


    tokens__removeSpace(Tokens,   Tokens2), 
    !, % throw all other tokenization away

    % DBG
    print(Tokens2),
    nl,


    phrase(expr(ParseTreeA), Tokens2),
    !,
    
    % DBG
    print(ParseTreeA),
    nl,
    
    
    % we need to fold the parsing tree to get a more useful representation
    fold(ParseTreeA,   ParseTreeB),
    !,
    
    % DBG
    print(ParseTreeB),
    nl,


    convParseTreeToAst(ParseTreeB,   Ast__result),
    !,


    % DBG
    print(Ast__result),
    nl,


    true.





manualtest_parserA :-

    Str__srcMetta = '(add2 5 7)',

    parserForMetta(Str__srcMetta,   Ast__result),
    
    true.

% manualtest run with
% ?- manualtest_parserA.

