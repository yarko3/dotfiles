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

fzf_install() {
  echo "Installing fzf..."
  "$DOTFILES_DIR"/fzf/install --key-bindings --completion --no-update-rc
  echo "Finished installing fzf"
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

install() {
  fzf_install
  fonts_install
  local_install
  rainbarf_install
}

create_ssh
change_to_zsh
install
