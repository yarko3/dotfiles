#!/bin/sh

if ! which -s brew 2>&1 ; then
    echo "brew not installed"
fi

brew update
brew upgrade

brew cask install \
    docker \
    iterm2 \
    java \
    spectacle \
    vagrant \
    virtualbox

brew install \
    bash-completion \
    boost \
    checkstyle \
    clang-format \
    cmake \
    coreutils \
    cppcheck \
    ctags \
    doxygen \
    gcc \
    git \
    gnome-common \
    grip \
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

brew cleanup
