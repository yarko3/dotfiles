#!/bin/sh

if ! which -s brew 2>&1 ; then
    echo "brew not installed"
fi

brew update
brew upgrade

brew install \
    bash-completion \
    boost \
    cmake \
    coreutils \
    cppcheck \
    ctags \
    doxygen \
    gcc \
    git \
    gnome-common \
    htop \
    hub \
    llvm \
    maven \
    neovim \
    node \
    python \
    shellcheck \
    tmux \
    tree \
    vim \
    watch \
    zsh

brew cask install \
    docker \
    java \
    spectacle \
    vagrant \
    virtualbox

brew cleanup
