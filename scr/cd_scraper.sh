#!/bin/bash
if [ -z $1 ]
then
    cd ~/mbig
else
    mbig=~/mbig/*
    for d in $mbig
    do
        if [[ $d =~ $1 ]]; then
            cd ~/mbig/$1
            exit
        fi
    done
fi
