#!/bin/bash
# Small test to make sure that I don't declare the same package install
# in multiple recipes (cuts down on dead code / install time).

packages=$(grep -ho "package '.*'" ./*.rb | sort | uniq -c | grep '^\s*[2-9]')

if [ -z "$packages" ]; then
    echo "No package multiply defined."
    exit 0
else
    echo "The following packages are multiply defined:"
    echo "$packages"
    exit 1
fi
