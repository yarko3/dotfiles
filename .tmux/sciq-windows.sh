#!/bin/bash
tmux new-window
tmux rename-window SCIQ

tmux split-window -t 0 -h
tmux split-window -t 0 -v
tmux split-window -t 1 -v

tmux send-keys -t 0 '/bb/bin/getprdwin -i' enter
sleep 3
tmux send-keys -t 1 '/bb/bin/getprdwin -i' enter
sleep 3
tmux send-keys -t 2 '/bb/bin/getprdwin -i' enter
sleep 3
tmux send-keys -t 3 '/bb/bin/getprdwin -i' enter
