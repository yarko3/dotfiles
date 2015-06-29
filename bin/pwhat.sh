#!/bin/bash
if [ -z $1 ]; then
    echo "No task specified, getting prdwin instead"

    if [ -z $DISPLAY ]; then
        . ~/scr/display/import_disp.sh
    fi

    /bb/bin/getprdwin -i
    return
fi

eval pwhat /scrp/bin/$1.tsk
