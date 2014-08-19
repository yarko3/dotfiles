#!/bin/bash
echo "Called the test git-hook script"
echo ""

# Make for loop split on newline character
IFS="
"

# Reject the commit if the diff added a certain string or regex
# Proof of concept: don't let people name a pattern _NEW
#if [ -n "$(git diff-index --cached -S '_NEW' HEAD --)" ]; then
#    echo "HOOK REJECTION - The following lines contain unacceptable words:"
#
#    for line in `git diff --cached -S '_NEW' HEAD | sed '/^[^+]/d'` ; do
#        if [[ $line =~ "_NEW" ]]; then
#            echo $line
#        fi
#    done
#
#    #exit 1
#fi

echo ""

# If the diff contains trailing whitespace, identify and fix it
if [ -n "$(git diff-index --cached --check HEAD --)" ]; then
    echo "HOOK REJECTION - The following lines contain trailing whitespace:"

    for line in `git diff --cached --check HEAD | sed '/^[^+]/d'` ; do
        echo $line
    done

    # TODO - Fix the whitespace automatically with sed and re-add it to
    # staging before the commit :D
    exit 1
fi


# TODO - Automatic integration with --clang-format?
if [ -n "" ]; then
    echo "HOOK Running clang-format"
    eval "git diff -U0 --cached HEAD | clang-format-diff.py -i -pl"
fi

exit 0
