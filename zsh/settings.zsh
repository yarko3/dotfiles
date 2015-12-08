if [ "$TERM" != "screen-256color" ]; then
    export TERM=xterm-256color
fi

export EDITOR=vi
export GTEST_COLOR=yes

# Vim mode
bindkey -v

# Reduce delay to 0.1 seconds for switching to normal mode with ESC
export KEYTIMEOUT=1

# Command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

export PATH="/home/$USER/bin"
export PATH="$PATH:/home/$USER/bin_local"
export PATH="$PATH:/bin"
export PATH="$PATH:/home/$USER/.rvm/bin"
export PATH="$PATH:/sbin"
export PATH="$PATH:/usr/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:/usr/local/sbin"
export PATH="$PATH:/usr/sbin"

export PYTHONPATH="/home/$USER/bin"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Hitting ctrl+r for nice history searching
bindkey "^r" history-incremental-search-backward
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Arrow keys, so other people can use my terminal
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# Unmap ctrl-s as "stop flow"
stty stop undef
