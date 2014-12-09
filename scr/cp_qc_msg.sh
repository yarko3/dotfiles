#!/bin/bash
if [ -z $1 ]; then
    echo "Pass the filename please."
    exit 1;
fi

if ! [ -f /scrp/data/scraper/testdata/test/$1 ]; then
    echo "Failed to find file $1"
    exit 1;
fi

cp /scrp/data/scraper/testdata/test/$1 .
ls -lrt
