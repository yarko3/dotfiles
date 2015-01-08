#!/bin/bash
if [ -z $1 ]; then
    cd ~/mbig
    return
fi

mbig=~/mbig/*
for d in $mbig
do
    if [[ $d =~ $1 ]]; then
        cd ~/mbig/$1
        return
    fi
done

cd ~/mbig/scrape.git/msgscrape/$1
