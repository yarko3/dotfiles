#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return

## ============================================================================
##                                  General
## ============================================================================
export TERM=xterm-256color
export EDITOR=vi

# update winsize after each command for better line-wrapping
shopt -s  checkwinsize

# Source my generic aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

# Ruby Version Manager
[ -f ~/.rvm/scripts/rvm ] && source ~/.rvm/scripts/rvm

# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '

LOCATION=$(uname -n)
## ============================================================================
##                           Bloomberg Environment
## ============================================================================
if [ "$BBENV" -a ! "$BBENV" == "1" ]; then
    . ~/bb_dotfiles/.bashrc_bb
elif [[ "$LOCATION" =~ bhipple ]]; then
    . ~/bb_dotfiles/.bashrc_bb_vm
## ============================================================================
##                             Home Environment
## ============================================================================
else
    echo "Running Home BASHRC"
    umask 0022

    export PATH="$PATH:~/bin:/usr/class/cs143/cool/bin"
    export CC=/usr/bin/clang
    export CXX=/usr/bin/clang++

    # Source my home aliases
    [ -f ~/.bash_aliases_home ] && . ~/.bash_aliases_home
fi


export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
