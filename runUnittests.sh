#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
set -e
# Treat unset variables as an error.
set -u

# The directory containing the test files
UNITTESTS_DIR="./unittestMetta"

echo "Starting to run ./eomc.sh for all files in $UNITTESTS_DIR"
echo "--------------------------------------------------"

# Check if the directory exists
if [ ! -d "$UNITTESTS_DIR" ]; then
    echo "Error: Directory '$UNITTESTS_DIR' not found." >&2
    exit 1
fi


# Loop through all files in the directory
# The '*' glob expands to all files and directories in the folder
for file_path in "$UNITTESTS_DIR"/*; do
    # Check if it's a regular file (not a directory or other special file)
    if [ -f "$file_path" ]; then
        echo ""

        echo "Processing file: $file_path"



        # Run eomc.sh compiler with the full path to the file as an argument
        "./eomc.sh" "$file_path"
        
        # run generated prolog code with swi-prolog
        swipl -s generated0.prolog -g entry0 -g halt
        
        # Check the exit status of a0.sh
        if [ $? -ne 0 ]; then
            echo "!!! ERROR: $A0_SCRIPT failed for $file_path (exit status: $?) !!!"
            exit 1
        fi
    fi
done

echo "Finished processing all files."
