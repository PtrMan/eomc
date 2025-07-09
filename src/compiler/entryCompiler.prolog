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
    parserForMetta(Str__srcMetta,   Ast__root),


    % * generate code with the prolog backend



    % MettaFunctionDef0 = mettaFunctionDefinition('exampleFunctionA', 2, Ast__result),

    % HACKY : for now we assume only one function declaration in the metta code
    Ast__functionDeclaration = Ast__root,

    
    Ctx0 = ctx(0),
    
    
 

    emitPrologFunctionOfMettaFunctionDefinition(Ast__functionDeclaration, Ctx0, Ctx1, Str__SrcProlog__generated, Int__PredicateIdRes),
    

    format('\n\n\n~w\n', [Str__SrcProlog__generated]),


    Str__pathToPrologRuntimeSrc = './src/runtime/runtimeProlog.prolog',
    read_file_to_string(Str__pathToPrologRuntimeSrc, Str__srcProlog__runtime, []),

    % bundle up with runtime
    strConcat([Str__srcProlog__runtime, '\n', Str__SrcProlog__generated],   Str__srcProlog__output),


    Str__pathOfOutput = 'generated0.prolog',
    write_string_to_file(Str__pathOfOutput, Str__srcProlog__output),



    true.




