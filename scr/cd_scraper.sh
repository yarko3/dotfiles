#!/bin/bash
if [ -z $1 ]; then
    echo "Specify a subdirectory of msgscrape"
    exit 1;
fi

prefix=~/mbig/scrape.git/msgscrape
dir=$1

# Second and third git workdirs
if [[ $2 == '2' ]]; then
    prefix=~/mbig/secondscrape.git/msgscrape
elif [[ $2 == '3' ]]; then
    prefix=~/mbig/thirdscrape.git/msgscrape
fi

# Top level change
#if [[ $1 == '1' || $1 == 'git' ]]; then
#    dir=
#else if [[ $1 == '2' ]]; then
#    dir=
#    prefix=~/mbig/secondscrape.git
#else if [[ $1 == '3' ]]; then
#    dir=
#    prefix=~/mbig/thirdscrape.git
#fi

# pcs_xb_mapping subdirectory services
if [ $dir == 'scrpxbsvc' ]; then
    dir=pcs_xb_mapping/$dir
elif [ $dir == 'scrpxbdb' ]; then
    dir=pcs_xb_mapping/db
fi

eval cd $prefix/$dir
