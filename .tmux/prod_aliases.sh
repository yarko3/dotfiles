#!/bin/bash
tmux send-keys "alias ll='ls -lrt' && alias less='less -n'" enter

# Frank's aliases to consider
    #PS1='`uname`-${HOSTNAME}:\${PWD} \$ ' && \
#alias lsd="ls -la | grep ^d"
#if [[ "$ARCH" = AIX* ]]; then alias ll='ls -lrt'; fi
#alias PS="ps -fe | fgrep -v grep | egrep -w"
#if [[ "$ARCH" = SunOS* ]];
#then alias PS="ps -fe | fgrep -v grep | egrep "; fi
#alias gl="gl -D"}}
