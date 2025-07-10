
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

#### limitations / assumptions

* compiler: there is virtually no semantics checking. The code has to be sematically correct so that the compiler produces correct output.
* runtime: there is virtually no stdlib implemented.
* only the simplistic bare minimal features of the language are supported for now. That means no `match` etc.

#### support table of compiler+libraries

| feature | parser | backend |
| :--- | :--- | :--- |
| datatype:boolean | yes | yes |
| datatype:int | yes | yes |
| datatype:float | yes | yes |
| function declaration | partial(one) | partial(one) |
| function invocation | yes | yes |
| variable definition/usage | yes | yes |
| control flow: conditionals | yes | yes |
| control flow: sequential | no | no |
| data flow: let | yes | yes |
| datastructure declaration | no | no |
| datastructure unification | - | no |

| feature | parser | runtime |
| :--- | :--- | :--- |
| basic math (+, *, etc.) | - | yes |
| basic math (cos, etc.) | - | (yes) |
| comparison | - | yes |
| match function | - | no |
| call into python | - | no |


#### supported Prolog environments

for the compiler swi-prolog is supported. Backend swi-prolog is supported and recommended. Other Prolog environments such as GNU-prolog could also work.
