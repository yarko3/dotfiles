echo "Loading zshrc"
if [ -f ~/.zsh_local/zshrc_local_before.zsh ]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

source ~/.zsh/plugins.zsh
source ~/.zsh/functions.zsh
source ~/.zsh/settings.zsh
source ~/.zsh/aliases.zsh
source ~/.zsh/exports.zsh

if [ -f ~/.zsh_local/zshrc_local_after.zsh ]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi

pathDeduplicate
