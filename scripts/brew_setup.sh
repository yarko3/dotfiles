#!/bin/sh

if ! which -s brew 2>&1 ; then
    echo "brew not installed"
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
brew install shellcheck
brew install python

brew cask install java
brew cask install spectacle
brew cask install vagrant
brew cask install virtualbox

brew cleanup
