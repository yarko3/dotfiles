#!/bin/bash
if [ -z $1 ]; then
    echo "Specify a service name"
    return
fi

eval cd /scrp/data/$1/log
clear

# List, reverse sort order, sort by time modified
ls -lrt
eval ls -lrt | tail -n 1 | awk {'print $9'} | xargs less
