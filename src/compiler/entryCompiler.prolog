:- consult('parserMetta.prolog').
:- consult('backendProlog.prolog').


% read_file_to_string(Str__pathToMettaSrc, Str__srcMetta, []).

entryCompiler :-
    Str__pathToMettaSrc = './a0.metta',

    read_file_to_string(Str__pathToMettaSrc, Str__srcMetta, []),

    print(Str__srcMetta),
    nl,

    % * parse MeTTa
    parserForMetta(Str__srcMetta,   Ast__result),


    % * generate code with the prolog backend
    

    true.
