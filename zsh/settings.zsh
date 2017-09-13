#!/usr/bin/env zsh

## ============================================================================
##                                 Settings
## ============================================================================
# Vim mode
bindkey -v

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

#  ============================================================================
#                            Configure Plugins
#  ============================================================================
# zsh-autosuggestions
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
export PROMPT='%{$fg[green]%}%n@%m %{$fg[cyan]%}%c%{$fg[yellow]%}$(git_prompt_info_mine)%{$reset_color%} $ '

export ZSH_THEME_GIT_PROMPT_PREFIX=""
export ZSH_THEME_GIT_PROMPT_SUFFIX=""

#  ============================================================================
#                               FZF Config
#  ============================================================================
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

