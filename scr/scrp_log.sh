#!/bin/bash
if [ -z $1 ]; then
    echo "Specify a service name"
    exit 1;
fi

eval cd /scrp/data/$1/log
clear
ls
