#!/bin/bash
echo "Called the test git-hook script"
echo ""

# Make for loop split on newline character
IFS="
"

# Reject the commit if the diff contains a certain string or regex
if [ -n "$(git diff-index --cached -S brogramming HEAD --)" ]; then
    echo "HOOK REJECTION - The following lines contain unacceptable words:"

    for line in `git diff --cached -S brogramming HEAD | sed '/^[^+-]/d'` ; do
        if [[ $line =~ "brogramming" ]]; then
            echo $line
        fi
    done

    #exit 1
fi

echo ""

# If the diff contains trailing whitespace, identify and fix it
if [ -n "$(git diff-index --cached --check HEAD --)" ]; then
    echo "HOOK REJECTION - The following lines contain trailing whitespace:"

    for line in `git diff --cached --check HEAD | sed '/^[^+]/d'` ; do
        echo $line
    done

    exit 1
fi

exit 0
