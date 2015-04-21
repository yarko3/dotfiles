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

# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '

LOCATION=$(uname -n)
## ============================================================================
##                           Bloomberg Environment
## ============================================================================
if [ "$BBENV" -a ! "$BBENV" == "1" ]; then
    echo "Running Bloomberg BASHRC"
    umask 0002

    # if chimera generated aliases exist, pull them into the current ENV
    [ -f ~/.bbalias ] && . ~/.bbalias

    # Autocomplete for Git
    GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
    [ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

    export APPROVER_UUID=$(/bb/bin/bbempinf.tsk -u fkolarek)

    # Build with clang
    #export INSTRUMENT_CC=/bbsrc/bin/clang_transform.pl
    #export INSTRUMENT_CPP=/bbsrc/bin/clang_transform.pl

    # Source my Bloomberg aliases
    [ -f ~/.bash_aliases_bb ] && . ~/.bash_aliases_bb

    # Only use this when not running on the VM
    alias git='vastool kinit -R && git'
    alias make='vastool kinit -R && make'

elif [[ "$LOCATION" =~ bhipple ]]; then
    echo "Running Bloomberg VM BASHRC"
    [ -f ~/.bash_aliases_bb ] && . ~/.bash_aliases_bb
    [ -f ~/nfs/.vimrc ] || sshfs-home
    alias gmake='toolkit-remote nylxdev2 gmake'
    export BBENV=1
## ============================================================================
##                             Home Environment
## ============================================================================
else
    echo "Running Home BASHRC"
    umask 0022

    export PATH="$PATH:~/bin:/usr/class/cs143/cool/bin"

    # Source my home aliases
    [ -f ~/.bash_aliases_home ] && . ~/.bash_aliases_home
fi

