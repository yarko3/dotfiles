#!/bin/bash
res=$(git rev-parse --show-toplevel 2>/dev/null)
if [ $? -eq 0 ]; then
    name=$(basename "$res")
else
        name=$(basename "$(pwd)")
fi
tmux rename-window "$name"

#git rev-parse --show-toplevel | xargs basename
#if [ -f ~/bin/tmux]
