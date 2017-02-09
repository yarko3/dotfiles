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

## ignore commands that:
##   * begin with a space character
##   * that is the same as the previous
## and remove duplicate commands
export HISTCONTROL=ignoreboth:erasedups

## ignore these commands in history
export HISTIGNORE="ls:cd:cd -:[bf]g:pwd:exit:clear:* --help:* -h"

## always append history to history file after each command
export PROMPT_COMMAND='history -a'

## keep up to 32^3 lines of history
export HISTFILESIZE=32768
export HISTSIZE="$HISTFILESIZE"

## save muti-line command as one history entry
shopt -s cmdhist

## if hash lookup fails, default to $PATH
shopt -s checkhash

## append history rather than overwrite it
shopt -s histappend

## allow re-edit of failed history substitution
shopt -s histreedit

## load history subsitution into editing buffer
shopt -s histverify

## don't start auto-completion if there is nothing on the command line
shopt -s no_empty_cmd_completion

## bind space to be magic
bind Space:magic-space

if [ "4" = "${BASH_VERSINFO[0]}" ]; then
    ## auto-correct minor typos on directory word completion
    shopt -s dirspell

    ## recursive globbing (e.g., ls **/*.txt)
    shopt -s globstar

    if [[ 2 -eq "${BASH_VERSINFO[1]}" && 29 -le "${BASH_VERSINFO[2]}" ]]; then
        ## expand directory names (e.g., $HOME => /home/UserName)
        shopt -s direxpand
    fi
fi

## add my own stuff to bin
PATH=~/bin:$PATH

# =============================================================================
#                                  Visual
# =============================================================================
# To have colors for ls and all grep commands such as grep, egrep and zgrep
export CLICOLOR=1
export GREP_OPTIONS='--color=auto'

# Prompt
PS1='\[\e]0;\u@\h:\w\a\]\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[36m\]\W\[\033[33m\]$(__git_ps1 " <%s>")\[\033[00m\] $ '
# old one
# PS1='\[\e]0;\u@\h:\w\a\][\u@\h \W$(__git_ps1 " (%s)")]\$ '

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\e[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\e[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\e[0m'           # end mode
export LESS_TERMCAP_se=$'\e[0m'           # end standout-mode
export LESS_TERMCAP_ue=$'\e[0m'           # end underline
export LESS_TERMCAP_us=$'\e[04;38;5;146m' # begin underline]]]]]]]'

# =============================================================================
#                                  Aliases
# =============================================================================
# Source my generic aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases

[ -f ~/.zsh/aliases.zsh ] && source ~/.zsh/aliases.zsh

# =============================================================================
#                                Completion
# =============================================================================
# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# to help with autocompletion for g
if [[ -n $(type -t _completion_loader) ]]; then
    _completion_loader git
fi
if [[ -n $(type -t __git_complete) ]]; then
    __git_complete g _git
fi
