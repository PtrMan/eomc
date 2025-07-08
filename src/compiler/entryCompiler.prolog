:- consult('parserMetta.prolog').
:- consult('backendProlog.prolog').





% write_string_to_file(+FileName, +Content)
%
% Writes the string Content to the file FileName, overwriting it if it exists.
write_string_to_file(FileName, Content) :-
    setup_call_cleanup(
        open(FileName, write, Stream),      % 1. Setup: Open the file for writing
        write(Stream, Content),             % 2. Call: Write the content to the stream
        close(Stream)                       % 3. Cleanup: Always close the stream
    ).


entryCompiler :-
    Str__pathToMettaSrc = './a0.metta',

    read_file_to_string(Str__pathToMettaSrc, Str__srcMetta, []),

    print(Str__srcMetta),
    nl,

    % * parse MeTTa
    parserForMetta(Str__srcMetta,   Ast__result),


    % * generate code with the prolog backend



    MettaFunctionDef0 = mettaFunctionDefinition('exampleFunctionA', 2, Ast__result),
    
    
    Ctx0 = ctx(0),
    
    
    % args of the entry predicate
    List__EntryPredicateArgs = ['A', 'B'],
    

    emitPrologFunctionOfMettaFunctionDefinition(MettaFunctionDef0, Ctx0, Ctx1, List__EntryPredicateArgs, Str__SrcProlog__dest, Int__PredicateIdRes),
    
    nl,
    nl,
    nl,
    print(Str__SrcProlog__dest),
    nl,


    Str__pathOfOutput = 'generated0.prolog',
    write_string_to_file(Str__pathOfOutput, Str__SrcProlog__dest),



    true.




