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
  echo "Installing nerd-fonts..."
  if ! [ -d ~/.local/share/fonts/NerdFonts ]; then
    "$DOTFILES_DIR"/nerd-fonts/install.sh
  else
    echo "nerd-fonts already installed"
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
    echo "dependency_install: choose 'sudo' mode when installing"
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
    git clone https://github.com/neovim/neovim ~/neovim
    cd ~/neovim
    git checkout "release-0.11"
    make CMAKE_EXTRA_FLAGS="-DCMAKE_INSTALL_PREFIX=$HOME/neovim" CMAKE_BUILD_TYPE="RelWithDebInfo"
    make install
    cd -

    # install python dependencies
    sudo apt install python3-neovim
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
