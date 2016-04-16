#!/bin/sh
chmod -R 0700 ghc

if [ -z "$MACOS" ]; then
    sudo chown -R $USER:$USER /home/$USER/dotfiles
fi
