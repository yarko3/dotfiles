#! /usr/bin/env bash

DOTFILES_DIR="$HOME/.dotfiles"

change_to_zsh() {
  if [ "$(echo "$SHELL" | grep -c "zsh")" -eq "0" ]; then
    echo "Setting shell to zsh"
    chsh -s "$(which zsh)"
  else
    echo "zsh is already the default shell"
  fi
}

create_ssh() {
  mkdir -p "$HOME"/.ssh
  chmod 0700 "$HOME"/.ssh
}

fonts_install() {
  echo "Installing fonts..."
  if ! [ -d ~/.local/share/fonts ]; then
    "$DOTFILES_DIR"/fonts/install.sh
  else
    echo "Fonts already installed"
  fi
  echo "Finished fonts"
}

local_install() {
  echo "Installing local packages..."
  if [ "$(uname)" = "Linux" ]; then
    "$DOTFILES_DIR"/scripts/apt_setup.sh
  else
    echo "No local packages to install..."
  fi
  echo "Finished installing local packages"
}

rainbarf_install() {
  echo "Installing rainbarf..."
  if ! [ -x ~/lib/perl5/bin/rainbarf ]; then
    echo "choose 'sudo' when installing"
    cpan Module::Build
    cd "$DOTFILES_DIR"/rainbarf
    perl Build.PL --install_base ~/lib/perl5
    ./Build test
    ./Build install
    cd -
  else
    echo "rainbarf already installed"
  fi
  echo "Finished installing rainbarf locally"
}

shfmt_install() {
  echo "Installing shfmt..."
  if [ -x ~/go/bin/shfmt ]; then
    echo "shfmt already installed"
    return
  fi

  if ! hash go; then
    echo "go executable not found!"
    return
  fi

  go get -u mvdan.cc/sh/cmd/shfmt
}

neovim_install() {
  echo "Installing neovim..."
  if ! [ -d ~/neovim ]; then
    echo "Installing neovim..."
    git clone https://github.com/neovim/neovim ~/neovim
    cd ~/neovim
    make CMAKE_EXTRA_FLAGS="-DCMAKE_INSTALL_PREFIX=$HOME/neovim"
    make install
    cd -

    # install python dependencies
    python3 -m pip install --upgrade pynvim
  else
    echo "neovim already installed"
  fi
  echo "Finished installing neovim locally"
}

install() {
  fonts_install
  local_install
  rainbarf_install
  shfmt_install
  neovim_install
}

create_ssh
change_to_zsh
install
