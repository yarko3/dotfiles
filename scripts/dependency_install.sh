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
    if ! [ -f ~/.fzf.zsh ]; then
        echo "Installing fzf"
        "$DOTFILES_DIR"/fzf/install --key-bindings --completion --no-update-rc
    else
        echo "fzf already installed"
    fi
}

fonts_install() {
    if ! [ -d ~/.local/share/fonts ]; then
        "$DOTFILES_DIR"/fonts/install.sh
    else
        echo "fonts already installed"
    fi
}

local_install() {
    echo "Installing local packages..."
    if [ "$(uname)" = "Darwin" ]; then
        "$DOTFILES_DIR"/scripts/brew_setup.sh
    else
        "$DOTFILES_DIR"/scripts/apt_setup.sh
    fi
    echo "Finished installing local packages"
}

nix_install() {
    if ! [ -d /nix ]; then
        echo "NixPkg not installed on this machine."
        if groups | grep -q sudo; then
            curl https://nixos.org/nix/install | sh
        else
            echo "Can't install NixPkg without sudo."
            return
        fi
    fi

    [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]] && . ~/.nix-profile/etc/profile.d/nix.sh

    CHANNEL="nixpkgs"
    nix-env -j 4 -iA "$CHANNEL.devEnv" "$CHANNEL.pyEnv"
}

create_ssh
fzf_install
fonts_install
local_install
nix_install
change_to_zsh
