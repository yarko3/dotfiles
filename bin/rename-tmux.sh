#!/bin/bash
res=$(git -C "$1" rev-parse --show-toplevel 2>/dev/null)
if [ $? -eq 0 ]; then
    basename "$res"
else
    basename "$1"
fi

#git rev-parse --show-toplevel | xargs basename
#if [ -f ~/bin/tmux]
