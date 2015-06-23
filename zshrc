# Allow local customizations in the ~/.zshrc_local_before file
if [ -f ~/.zsh_local/zshrc_local_before.zsh ]; then
    source ~/.zsh_local/zshrc_local_before.zsh
fi

# External plugins
source ~/.zsh/plugins.zsh

# Settings
source ~/.zsh/settings.zsh

# Bootstrap
source ~/.zsh/bootstrap.zsh

# Aliases
source ~/.zsh/aliases.zsh

# Allow local customizations in the ~/.zshrc_local_after file
if [ -f ~/.zsh_local/zshrc_local_after.zsh ]; then
    source ~/.zsh_local/zshrc_local_after.zsh
fi
