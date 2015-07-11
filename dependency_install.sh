#!/bin/bash

###############################
# Software package dependencies
#
sudo apt-get install -y \
awesome \
build-essential \
cmake \
tmux \
vim-gtk \
xclip \
xsel \
zsh \

##########################################
# Change shell to zsh, if not already done
#
if [ $(echo "$SHELL" | grep -c "zsh") -eq "0" ]; then
    echo "Setting shell to zsh"
    chsh -s $(which zsh)
else
    echo "zsh is already the default shell"
fi

#############################################
# Create ssh dir with appropriate permissions
#
mkdir -p $HOME/.ssh
chmod 0600 $HOME/.ssh
