#!/bin/bash
# This crazy hack runs the vundle command silently, without needing tty or user input
# See https://github.com/VundleVim/Vundle.vim/issues/511
echo "Installing Vundle plugins."
echo | vim +PluginInstall +PluginUpdate +qall &>/dev/null
echo "Vundle installation complete."
