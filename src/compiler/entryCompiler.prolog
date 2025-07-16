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
    parserForMetta(Str__srcMetta,   List__ast),


    % * generate code with the prolog backend

    ( true -> format("compile trace: gen code with prolog backend\n") ; true),





    
    Ctx0 = ctx(0),
    
    
    % HACK : we hardcoded the set of functionnames for now
    Set__Str__functionnames = ['+','-','*','/', 'cos', 'sin', 'exp', 'log', 'tan', 'tanh',  '<', '>', '==',   'index-atom', 'car-atom', 'cdr-atom',   'superpose', 'collapse'],

    format("TRACE : GenFn: generate definitions of functions\n"),


    emitPrologFunctionOfMettaFunctionDefinitions(List__ast, Ctx0, Ctx1, Set__Str__functionnames,    Str__SrcProlog__generated),

    format('\n\n\n~w\n', [Str__SrcProlog__generated]),

    format("TRACE : GenFn: done\n"),


    Str__pathToPrologRuntimeSrc = './src/runtime/runtimeProlog.prolog',
    read_file_to_string(Str__pathToPrologRuntimeSrc, Str__srcProlog__runtime, []),


    % HACK : entry code with hardcoded function to call into
    Str__srcProlog__entry = "\n\n\nentry0 :-\n format(\"TRACE: ENTRY\\n\"),\n pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['examplefunctiona', 10]), _,   Res),\n format(\"~w\\n\", [Res]),\n true.\n",


    % bundle up with runtime and code for entry
    strConcat([Str__srcProlog__runtime, '\n', Str__SrcProlog__generated,  Str__srcProlog__entry],   Str__srcProlog__output),


    Str__pathOfOutput = 'generated0.prolog',
    write_string_to_file(Str__pathOfOutput, Str__srcProlog__output),



    true.




