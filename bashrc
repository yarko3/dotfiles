#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return
echo "Loading bashrc"

if [ -f ~/.bash_local/bash_local_before.zsh ]; then
    source ~/.bash_local/bash_local_before.zsh
fi

source ~/.bash/functions.sh
source ~/.bash/settings.sh
source ~/.bash/aliases.sh

if [ -f ~/.bash_local/bash_local_after.zsh ]; then
    source ~/.bash_local/bash_local_after.zsh
fi

