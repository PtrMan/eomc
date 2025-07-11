#!/bin/bash

# Execute the swipl command
swipl -s ./src/compiler/entryCompiler.prolog "$@" -g entryCompiler -g halt

# Exit with the same status code as the swipl command
exit $?
