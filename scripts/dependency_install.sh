#! /usr/bin/env bash

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
        ./fzf/install --key-bindings --completion --no-update-rc
    else
        echo "fzf already installed"
    fi
}

fonts_install() {
    if ! [ -d ~/.local/share/fonts ]; then
        ./fonts/install.sh
    else
        echo "fonts already installed"
    fi
}

change_to_zsh
create_ssh
fzf_install
fonts_install
