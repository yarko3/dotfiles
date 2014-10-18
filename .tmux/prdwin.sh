#!/bin/bash
LOGIN_SECS=7
RACE_CONDITION_DELAY=0

function n290() {
    tmux new-window
    tmux rename-window N290
    tmux send-keys '/bb/bin/getprdwin -i' enter

    sleep LOGIN_SECS
    send_aliases
}

function sciq() {
    tmux new-window
    tmux rename-window SCIQ

    tmux split-window -t 0 -h
    tmux split-window -t 0 -v
    tmux split-window -t 1 -v

    for i in `seq 0 3`;
    do
        eval "tmux send-keys -t $i '/bb/bin/getprdwin -i' enter"
        sleep RACE_CONDITION_DELAY
    done

    sleep LOGIN_SECS
    send_alises
}
function scip {
    tmux new-window
    tmux rename-window SCIP

    tmux split-window -t 0 -h
    tmux split-window -t 1 -h
    tmux select-layout even-horizontal

    tmux split-window -t 0 -v
    tmux split-window -t 1 -v
    tmux split-window -t 2 -v

    for i in `seq 0 5`;
    do
        eval "tmux send-keys -t $i '/bb/bin/getprdwin -i' enter"
        sleep RACE_CONDITION_DELAY
    done

    sleep LOGIN_SECS
    send_alises
}

function send_aliases() {
    tmux setw synchronize-panes on
    tmux send-keys "alias ll='ls -lrt' && alias less='less -n'" enter
}

# Frank's aliases to consider
    #PS1='`uname`-${HOSTNAME}:\${PWD} \$ ' && \
#alias lsd="ls -la | grep ^d"
#if [[ "$ARCH" = AIX* ]]; then alias ll='ls -lrt'; fi
#alias PS="ps -fe | fgrep -v grep | egrep -w"
#if [[ "$ARCH" = SunOS* ]];
#then alias PS="ps -fe | fgrep -v grep | egrep "; fi
#alias gl="gl -D"}}


while getopts "nqpa" opt; do
    case $opt in
        a)
            echo "Sending aliases through tmux"
            send_aliases
            exit 0
            ;;
        n)
            echo "Getting n290 prdwin"
            n290
            exit 0
            ;;
        q)
            echo "Getting SCIQ prdwin" >&2
            sciq
            exit 0
            ;;
        p)
            echo "Getting SCIP prdwin" >&2
            scip
            exit 0
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done
