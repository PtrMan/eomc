
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

#### basic source code examples

source code is assumed to contain only one function declaration. Only integers and calls to functions are supported at this stage.

    (= (exampleFunctionA)  (*  (+ 2.2 3) 6) )

<br />

#### support table of compiler+libraries

| feature | parser | backend |
| :--- | :--- | :--- |
| datatype:boolean | yes | no |
| datatype:int | yes | yes |
| datatype:float | yes | yes |
| function declaration | partial(one) | partial(one) |
| function invocation | yes | yes |
| variable definition/usage | no | (yes) |
| conditionals | yes | yes |
| sequential | no | no |
| datastructure declaration | no | no |
| datastructure unification | - | no |

| feature | parser | runtime |
| :--- | :--- | :--- |
| basic math (+, *, etc.) | - | yes |
| basic math (cos, etc.) | - | no |
| comparison | - | no |
| call into python | - | no |


#### supported Prolog environments

for the compiler swi-prolog is supported. Backend swi-prolog is supported and recommended. Other Prolog environments such as GNU-prolog could also work.
