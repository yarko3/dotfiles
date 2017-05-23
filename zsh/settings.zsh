## ============================================================================
##                                 Settings
## ============================================================================
# Vim mode
bindkey -v

# Command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Visual modes added in zsh 5.0.8
visualModes=()
is-at-least 5.0.8 2>/dev/null
if [ $? -eq 0 ]; then
    visualModes=("visual")
fi

bindkey -M "vicmd" 'k' history-substring-search-up
bindkey -M "vicmd" 'j' history-substring-search-down

# Run `bindkey -l` to see a list of modes, and `bindkey -M foo` to see a list of commands active in mode foo
# Move to vim escape mode
bindkey -M "viins" jj vi-cmd-mode
bindkey -M "viins" kk vi-cmd-mode
bindkey -M "viins" jk vi-cmd-mode
bindkey -M "viins" kj vi-cmd-mode

# Unmap ctrl-s as "stop flow"
stty stop undef

# zsh-autosuggestions cfg
# # Bind <CTRL><SPC> to accept and execute
bindkey '^ ' autosuggest-accept

## ============================================================================
##                                  Prompt
## ============================================================================
git_prompt_info_mine() {
    git_prompt_text="$(git symbolic-ref HEAD 2>/dev/null | cut -d'/' -f3)" || ""
    if [[ -n $git_prompt_text ]]; then
        echo " <$git_prompt_text>"
    else
        echo ""
    fi
}
PROMPT='%{$fg[green]%}%n@%m %{$fg[cyan]%}%c%{$fg[yellow]%}$(git_prompt_info_mine)%{$reset_color%} $ '

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""

#  ============================================================================
#                               FZF Config
#  ============================================================================
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

