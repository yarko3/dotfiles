#!/bin/bash
# Quick script to run $ cd ~/mbig/scrape.git/msgscrape/<subdir>
if [ -z $1 ]; then
    echo "Specify a subdirectory of msgscrape"
    exit 1;
fi

eval cd ~/mbig/scrape.git/msgscrape/$1
