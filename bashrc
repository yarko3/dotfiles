#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return
echo "Loading bashrc"

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
PS1='\[\e]0;\u@\h:\w\a\][\u@\h \W$(__git_ps1 " (%s)")]\$ '

# Pull data out of the zsh files
if [ -f ~/.zsh_local/zshrc_local_before.zsh ]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

source ~/.zsh/aliases.zsh

if [ -f ~/.zsh_local/zshrc_local_after.zsh ]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi
