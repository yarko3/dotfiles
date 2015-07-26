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

[ -f ~/.zsh/aliases.zsh ] && . ~/.zsh/aliases.zsh
