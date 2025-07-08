
experimental optimizing MeTTa compiler.

Compiles MeTTa to a target language which is prolog. The target language output is slightly optimized.


#### how to install

install swi-prolog dependencies by running the prolog and confirming everything with "yes"

    swipl ./INSTALL.prolog

#### how to run

source source code is read from a0.metta

basic running

    swipl -s ./src/compiler/entryCompiler.prolog -g entryCompiler -g halt

running with dumping generated code to terminal

    swipl -s ./src/compiler/entryCompiler.prolog -g entryCompiler -g halt    && echo "" && cat generated0.prolog

