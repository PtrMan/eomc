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

    current_prolog_flag(argv, SwiplArgs), % get command line arguments

    write(SwiplArgs),nl,

    [Str__pathToMettaSrc|_] = SwiplArgs,


    %Str__pathToMettaSrc = './a0.metta',

    read_file_to_string(Str__pathToMettaSrc, Str__srcMetta, []),

    write(Str__srcMetta),nl,

    % * parse MeTTa
    parserForMetta(Str__srcMetta,   List__ast),


    % * generate code with the prolog backend

    ( true -> format("compile trace: gen code with prolog backend\n") ; true),





    
    Ctx0 = ctx(0),
    
    
    % hardcoded the set of functionnames
    Set__Str__functionnamesHardcoded = ['+','-','*','/', 'cos', 'sin', 'exp', 'log', 'tan', 'tanh',  '<', '>', '==',   'index-atom', 'car-atom', 'cdr-atom',   'let2', 'superpose', 'collapse',    'asserteq2', 'writeln2'],





    functionDefinitions__extractFunctionnames(List__ast,   List__str__functionnamesExtracted), % extract the functionnames

    format("DBG : extracted functionnames:\n"),
    write(List__str__functionnamesExtracted),nl,

    append(Set__Str__functionnamesHardcoded, List__str__functionnamesExtracted,   Set__Str__functionnames),


    format("TRACE : GenFn: generate definitions of functions\n"),


    emitPrologFunctionOfMettaFunctionDefinitions(List__ast, Ctx0, Ctx1, Set__Str__functionnames,    Str__SrcProlog__generated),

    format('\n\n\n~w\n', [Str__SrcProlog__generated]),

    format("TRACE : GenFn: done\n"),


    Str__pathToPrologRuntimeSrc = './src/runtime/runtimeProlog.prolog',
    read_file_to_string(Str__pathToPrologRuntimeSrc, Str__srcProlog__runtime, []),


    % HACK : entry code with hardcoded function to call into
    Str__srcProlog__entry = "\n\n\nentry0 :-\n format(\"TRACE: ENTRY\\n\"),\n pred__(runtimeCtx(), runtimeCtx(), mettaExpr(['examplefunctiona', 10]),   Res),\n format(\"~w\\n\", [Res]),\n true.\n",


    % bundle up with runtime and code for entry
    strConcat([Str__srcProlog__runtime, '\n', Str__SrcProlog__generated,  Str__srcProlog__entry],   Str__srcProlog__output),


    Str__pathOfOutput = 'generated0.prolog',
    write_string_to_file(Str__pathOfOutput, Str__srcProlog__output),

    halt, % end program

    true.




