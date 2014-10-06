#!/bin/bash
# Quick script to run $ pwhat /scrp/bin/<task_name>.tsk
if [ -z $1 ]; then
    echo "Specify a task name, without the .tsk extension"
    return 1;
fi

eval pwhat "/scrp/bin/$1.tsk"
