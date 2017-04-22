## ============================================================================
##                                  General
## ============================================================================
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
# Prompt
PS1='\[\e]0;\u@\h:\w\a\]\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[36m\]\W\[\033[33m\]$(__git_ps1 " <%s>")\[\033[00m\] $ '

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
  elif [ -f /etc/bash_completion.d/git ]; then
    . /etc/bash_completion.d/git
  elif [ -f /usr/local/etc/bash_completion ]; then
    . /usr/local/etc/bash_completion
  fi
fi

# to help with autocompletion for g
if [[ -n $(type -t _completion_loader) ]]; then
    _completion_loader git
fi

if [[ -n $(type -t __git_complete) ]]; then
    __git_complete g _git
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
