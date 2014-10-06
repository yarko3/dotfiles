#!/bin/bash
if [ "$#" -ne 2 ]; then
    echo "Usage: <display_number> <program>"
    exit 1;
fi

new_disp=":0.$1"

export DISPLAY=$new_disp
eval $2
