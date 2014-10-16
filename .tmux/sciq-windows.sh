#!/bin/bash
tmux new-window
tmux rename-window SCIQ

tmux split-window -t 0 -h
tmux split-window -t 0 -v
tmux split-window -t 1 -v

aliasString="alias ll='ls -lrth' && alias less='less -n'"


for i in `seq 0 3`;
do
    #eval "tmux send-keys -t $i '/bb/bin/getprdwin -i' enter"
    sleep 5
done

sleep 7
tmux setw synchronize-panes on
~/.tmux/prod_aliases.sh
