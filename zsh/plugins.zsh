# load zgen
# auto-fu expects us to be in emacs mode before sourcing (wtf)
bindkey -e
bindkey $'\e' vi-cmd-mode
source "${HOME}/.dotfiles/zgen/zgen.zsh"
bindkey -v

# Check only when there's no init script
# To regenerate zgen config from scratch, rm ~/.zgen/init.zsh
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # oh-my-zsh plugins
    zgen oh-my-zsh plugins/command-not-found
    zgen oh-my-zsh plugins/wd
    zgen oh-my-zsh plugins/web-search
    zgen oh-my-zsh plugins/tmux
    # zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/colorize
    zgen oh-my-zsh plugins/web-search

    # update zgen and plugins automatically
    zgen load unixorn/autoupdate-zgen

    # Github plugins
    zgen load djui/alias-tips
    zgen load hchbaw/auto-fu.zsh . pu
    zgen load rupa/z
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-syntax-highlighting

    # save all to init script
    zgen save
fi

