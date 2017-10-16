#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return
echo "Loading bashrc"

if [ -f ~/.bashrc_local/bashrc_local_before.sh ]; then
    source ~/.bashrc_local/bashrc_local_before.sh
fi

[ -f "$HOME"/.nix-profile/etc/profile.d/nix.sh ] && \
    . "$HOME"/.nix-profile/etc/profile.d/nix.sh

source ~/.bash/functions.sh
source ~/.bash/settings.sh
source ~/.bash/aliases.sh
source ~/.bash/exports.sh

if [ -f ~/.bashrc_local/bashrc_local_after.sh ]; then
    source ~/.bashrc_local/bashrc_local_after.sh
fi

pathDeduplicate
