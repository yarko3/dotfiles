#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return
echo "Loading bashrc"

if [ -f ~/.bash_local/bash_local_before.sh ]; then
    source ~/.bash_local/bash_local_before.sh
fi

source ~/.bash/functions.sh
source ~/.bash/settings.sh
source ~/.bash/aliases.sh
source ~/.bash/exports.sh

if [ -f ~/.bash_local/bash_local_after.sh ]; then
    source ~/.bash_local/bash_local_after.sh
fi
