# load zgen
source "${HOME}/.dotfiles/zgen/zgen.zsh"

# check if there's no init script
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # oh-my-zsh plugins
    #zgen oh-my-zsh plugins/chucknorris
    zgen oh-my-zsh plugins/command-not-found
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/wd
    zgen oh-my-zsh plugins/web-search

    # Github plugins
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-completions src

    # theme
    zgen oh-my-zsh themes/lambda

    # save all to init script
    zgen save
fi
