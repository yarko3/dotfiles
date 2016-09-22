echo "Loading zshrc"
if [ -f ~/.zsh_local/zshrc_local_before.zsh ]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

fpath+=(~/.zsh/completions $fpath)

source ~/.zsh/functions.zsh
source ~/.zsh/plugins.zsh
source ~/.zsh/settings.zsh
source ~/.zsh/aliases.zsh

if [ -f ~/.zsh_local/zshrc_local_after.zsh ]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi
