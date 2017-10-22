#!/bin/sh

if ! which brew > /dev/null 2>&1 ; then
    echo "brew not installed; installing..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew update
brew upgrade

brew cask install \
    iterm2 \
    spectacle

brew install \
    clang-format \
    curl \
    gcc \
    gnome-common \
    grip \
    llvm \
    neovim \
    zsh

brew cleanup
