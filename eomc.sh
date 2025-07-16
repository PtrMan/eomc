#!/bin/bash

# Execute the swipl command
swipl -s ./src/compiler/entryCompiler.prolog -g entryCompiler -- "$@"

# Exit with the same status code as the swipl command
exit $?
