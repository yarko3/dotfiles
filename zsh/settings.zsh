## ============================================================================
##                           Environment Variables
## ============================================================================
if [ "$TERM" != "screen-256color" ]; then
    export TERM=xterm-256color
fi

export GTEST_COLOR=yes
export EDITOR=vi

cmd=$(uname -a | grep -q "Darwin")
if [ $? -eq 0 ]; then
    export MACOS="true"
    export EDITOR=vim
fi

# Reduce delay to 0.1 seconds for switching to normal mode with ESC
export KEYTIMEOUT=1

PATH=~/bin
if [ -d /opt/bb/bin ]; then
    PATH=$PATH:/opt/bb/bin
fi
PATH=$PATH:~/bin_local
PATH=$PATH:~/.local/bin

if [ -n "$MACOS" ]; then
    PATH=$PATH:~/.stack/programs/x86_64-osx/ghc-7.10.3/bin
    PATH=$PATH:~/Applications/context/tex/texmf-osx-64/bin
else
    PATH=$PATH:~/.stack/programs/x86_64-linux/ghc-7.10.3/bin
fi

PATH=$PATH:~/.cabal/bin
PATH=$PATH:~/.rvm/bin
PATH=$PATH:/opt/chefdk/bin
PATH=$PATH:/bin
PATH=$PATH:/sbin
PATH=$PATH:/usr/local/bin
PATH=$PATH:/usr/bin
PATH=$PATH:/usr/local/sbin
PATH=$PATH:/usr/sbin

export PYTHONPATH=~/bin

if [ -d /opt/bb/bin ]; then
    PATH=$PATH:/opt/bb/bin
fi

export LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

. ~/bin/resty

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
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Arrow keys, so other people can use my terminal
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Unmap ctrl-s as "stop flow"
stty stop undef

# Vim mode
bindkey -v

## ============================================================================
##                                  Prompt
## ============================================================================
PROMPT='%{$fg[yellow]%}λ %m %{$fg[green]%}%c%{$fg[yellow]%} →  %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""

export SPARK_HOME="/home/brh/spark-1.6.0-bin-hadoop2.6"

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

