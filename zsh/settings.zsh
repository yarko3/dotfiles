export TERM=xterm-256color

# Vim mode
bindkey -v

# Reduce delay to 0.1 seconds for switching to normal mode with ESC
export KEYTIMEOUT=1

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

export PATH="/home/brh/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:~/bin:/usr/class/cs143/cool/bin:/home/brh/.rvm/bin:~/bin:/usr/class/cs143/cool/bin:/home/brh/.rvm/bin"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Hitting ctrl+r for nice history searching
bindkey "^r" history-incremental-search-backward
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

# Arrow keys, so other people can use my terminal
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
