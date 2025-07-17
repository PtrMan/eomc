experimental optimizing MeTTa compiler.

Compiles MeTTa to a target language which is prolog. The target language output is slightly optimized.


#### how to install

install swi-prolog dependencies by running the prolog and confirming everything with "yes"

    swipl ./INSTALL.prolog

#### how to run

source source code is read from a0.metta

basic running

    ./eomc.sh

running with dumping generated code to terminal

    ./eomc.sh    && echo "" && cat generated0.prolog

running with dumping generated code to terminal and executing generated program with swi-prolog

    ./eomc.sh && cat generated0.prolog     && swipl -s generated0.prolog -g entry0 -g halt

#### basic source code examples

    (= (exampleFunctionA)  (*  (+ 2.2 3) 6) )

<br />

#### limitations / assumptions

* compiler: there is virtually no semantics checking. The code has to be sematically correct so that the compiler produces correct output.
* lexer : the lexeer doesn't parse anything with `*`, `-` etc. correctly. so `let2` has to be used instead of `let*`  etc. for now.
* runtime: there is a very minimal stdlib implemented.
* only the simplistic bare minimal features of the language are supported for now. That means no `match`, `case` etc.

#### support table of compiler+libraries

| feature | parser | backend |
| :--- | :--- | :--- |
| datatype:boolean | yes | yes |
| datatype:int | yes | yes |
| datatype:float | yes | yes |
| function declaration | full | full |
| function invocation | yes | yes |
| variable definition/usage | yes | yes |
| control flow: conditionals | yes | yes |
| control flow: sequential | - | yes |
| data flow: let | yes | yes |
| datastructure declaration | yes | yes |
| datastructure unification | - | (yes) |

| feature | parser | runtime |
| :--- | :--- | :--- |
| basic math (+, *, etc.) | - | yes |
| basic math (cos, etc.) | - | (yes) |
| comparison | - | yes |
| collapse function | - | yes |
| superpose function | - | yes |
| match function | - | no |
| call into python | - | no |


#### supported Prolog environments

for the compiler swi-prolog is supported. Backend swi-prolog is supported and recommended. Other Prolog environments such as GNU-prolog could also work.

#### stable versions

here are version which are relativly stable

https://github.com/PtrMan/eomc/tree/59e0ae8bda0011ced181f50a80942c84b5f0206c from 07-17-2025
