#!/bin/bash
if [ -z $1 ]; then
    echo "Specify a task name, without the .tsk extension"
    exit 1;
fi

eval pwhat /scrp/bin/$1.tsk
