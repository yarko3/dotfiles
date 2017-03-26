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
# Command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Hitting ctrl+r for nice history searching
bindkey "^r" history-incremental-search-backward

for m in "vicmd" "afu-vicmd"; do
    bindkey -M vicmd 'k' history-substring-search-up
    bindkey -M vicmd 'j' history-substring-search-down
done

# Run `bindkey -l` to see a list of modes, and `bindkey -M foo` to see a list of commands active in mode foo
# Move to vim escape mode
for m in "viins" "afu"; do
    bindkey -M "$m" jj vi-cmd-mode
    bindkey -M "$m" kk vi-cmd-mode
    bindkey -M "$m" jk vi-cmd-mode
    bindkey -M "$m" kj vi-cmd-mode
done

# Unmap ctrl-s as "stop flow"
stty stop undef

# Vim mode
bindkey -v

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

## ============================================================================
##                              Auto-Fu Config
## ============================================================================
# https://github.com/hchbaw/auto-fu.zsh/issues/29
if [ "$AUTO_FU" = "skip" ]; then
    echo "Skipping auto-fu"
    return
fi

zle-line-init () { auto-fu-init; }; zle -N zle-line-init
zle -N zle-keymap-select auto-fu-zle-keymap-select
zstyle ':completion:*' completer _oldlist _complete
zstyle ':auto-fu:var' postdisplay $'
'

my-reset-prompt-maybe () {
  # XXX: While auto-stuff is in effect,
  # when hitting <Return>, $KEYMAP becomes to `main`:
  # <Return> → `reset-prompt`(*) → `accept-line` → `zle-line-init`
  # → `zle-keymap-select` → `reset-promt` (again!)
  # Skip unwanted `reset-prompt`(*).
  ((auto_fu_init_p==1)) && [[ ${KEYMAP-} == main ]] && return

  # XXX: Please notice that `afu` is treated as Insert-mode-ish.
  RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins|afu)/}"
  zle reset-prompt
}

zle-keymap-select () {
  auto-fu-zle-keymap-select "$@"
  my-reset-prompt-maybe
}
zle -N zle-keymap-select
