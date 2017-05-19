#!/usr/bin/env bash

which -s brew 2>&1
if [[ $? != 0 ]] ; then
    echo "brew not installed"
    exit
fi

brew update
brew upgrade

brew install bash-completion
brew install boost
brew install cmake
brew install coreutils
brew install ctags
brew install doxygen
brew install gcc
brew install git
brew install gnome-common
brew install htop
brew install hub
brew install llvm
brew install maven
brew install neovim
brew install node
brew install tmux
brew install tree
brew install vim
brew install zsh

brew cask install vagrant
brew cask install virtualbox


# Remove outdated versions from the cellar.
brew cleanup
