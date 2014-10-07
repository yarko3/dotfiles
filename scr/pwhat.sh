#!/bin/bash
if [ -z $1 ]; then
    echo "No task specified, getting prdwin instead"
    eval /bb/bin/getprdwin
    exit 1;
fi

eval pwhat /scrp/bin/$1.tsk
