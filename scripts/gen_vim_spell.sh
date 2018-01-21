#!/bin/bash
DOTFILES_DIR="$HOME/.dotfiles"

vim "+mkspell! $DOTFILES_DIR/vim/spell/extra-words.add" +qall
