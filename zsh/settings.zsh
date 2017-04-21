## ============================================================================
##                           Environment Variables
## ============================================================================
export ZSH_TMUX_AUTOSTART=true
export ZSH_TMUX_AUTOCONNECT=false
if [ "$TERM" != "screen-256color" ]; then
    export TERM=xterm-256color
fi

if [ -n "$SSH_CONNECTION" ]; then
    export ZSH_TMUX_AUTOSTART=false
fi

if [ $(uname) = "Darwin" ]; then
    export MACOS="true"
    export EDITOR=vim
fi

# Reduce delay to 0.1 seconds for switching to normal mode with ESC
export KEYTIMEOUT=1

if [ -d /opt/bb/bin ]; then
    PATH=/opt/bb/bin:$PATH
fi

PATH=~/bin:$PATH
PATH=~/bin_local:$PATH
PATH=$PATH:~/.local/bin

PATH=$PATH:/bin
PATH=$PATH:/sbin
PATH=$PATH:/usr/local/bin
PATH=$PATH:/usr/bin
PATH=$PATH:/usr/local/sbin
PATH=$PATH:/usr/sbin

[ -f ~/.config/hub ] && export GITHUB_STANDARD_TOKEN=$(grep oauth_token ~/.config/hub | awk '{print $2}')

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
is-at-least 5.0.8
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

# Hitting ctrl+r for nice history searching
bindkey "^r" history-incremental-search-backward

# Unmap ctrl-s as "stop flow"
stty stop undef

## ============================================================================
##                                  Prompt
## ============================================================================
git_prompt_info_mine() {
    git_prompt_text="$(git_prompt_info)"
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

