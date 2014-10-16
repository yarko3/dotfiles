#!/bin/bash
tmux new-window
tmux rename-window N290

tmux send-keys '/bb/bin/getprdwin -i' enter
sleep 7

~/.tmux/prod_aliases.sh
