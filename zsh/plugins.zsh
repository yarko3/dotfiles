# load zgen
# auto-fu expects us to be in emacs mode before sourcing (wtf)
bindkey -v
source "${HOME}/.dotfiles/zgen/zgen.zsh"

# Check only when there's no init script
# To regenerate zgen config from scratch, rm ~/.zgen/init.zsh
if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh

    # oh-my-zsh plugins
    zgen oh-my-zsh plugins/colorize
    zgen oh-my-zsh plugins/command-not-found
    zgen oh-my-zsh plugins/last-working-dir
    zgen oh-my-zsh plugins/wd
    zgen oh-my-zsh plugins/web-search

    # update zgen and plugins automatically
    zgen load unixorn/autoupdate-zgen

    # Github plugins
    zgen load djui/alias-tips
    zgen load rupa/z
    zgen load zsh-users/zsh-autosuggestions
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-history-substring-search
    zgen load zsh-users/zsh-syntax-highlighting

    # save all to init script
    zgen save
fi

