#!/bin/bash
# Small test to make sure that I don't declare the same package install
# in multiple recipes (cuts down on dead code / install time).

echo "Checking for multiple package definitions in Chef recipe."

packages=$(grep -ho "package '.*'" ./*.rb | sort | uniq -c)
duplicates=$(grep '^\s*[2-9]' "$packages")

if [ -z "$duplicates" ]; then
    echo "No package multiply defined."
    exit 0
else
    echo "The following packages are multiply defined:"
    echo "$duplicates"
    exit 1
fi
