#!/bin/sh

if [ -z "$MACOS" ]; then
    sudo chown -R $USER:$USER /home/$USER/dotfiles
fi
