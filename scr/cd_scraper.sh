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

prefix=~/mbig/scrape.git/msgscrape
dir=$1

# Second and third git workdirs
if [[ $2 == '2' ]]; then
    prefix=~/mbig/secondscrape.git/msgscrape
elif [[ $2 == '3' ]]; then
    prefix=~/mbig/thirdscrape.git/msgscrape
fi

cd $prefix/$dir
