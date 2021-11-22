export GTEST_COLOR=yes

export LIBRARY_PATH="/opt/X11/lib:$LIBRARY_PATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# To have colors for ls and all grep commands such as grep, egrep and zgrep
export CLICOLOR=1

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\e[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\e[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\e[0m'           # end mode
export LESS_TERMCAP_se=$'\e[0m'           # end standout-mode
export LESS_TERMCAP_ue=$'\e[0m'           # end underline
export LESS_TERMCAP_us=$'\e[04;38;5;146m' # begin underline]]]]]]]'

export ZSH_TMUX_AUTOSTART=false
export ZSH_TMUX_AUTOCONNECT=false

# know which tmux window to rename
if [ -n "$TMUX" ]; then
  export TMUX_WINDOW=$(tmux display-message -p '#I')
fi

export DISABLE_AUTO_TITLE="true"

if [ "$(uname)" = "Darwin" ]; then
    export MACOS="true"
fi

# set default editor (probably my lightweight vim masquerading as vi)
export EDITOR=vi

# Reduce delay for switching to normal mode with ESC
export KEYTIMEOUT=20

 export FZF_DEFAULT_OPTS=" \
  --inline-info \
  --reverse \
  --color=fg+:#F8F8F8,bg+:#515559,pointer:#F8F8F8,marker:226 \
  --bind=ctrl-e:select-all+accept \
  --bind=ctrl-d:half-page-down \
  --bind=ctrl-u:half-page-up
  --bind=ctrl-t:toggle+down
  --bind=ctrl-b:toggle+up
  --bind=ctrl-g:select-all+accept \
  "

PATH=~/bin:$PATH
PATH=~/bin_local:$PATH
PATH=$PATH:~/.local/bin
PATH=$PATH:~/lib/perl5/bin

# For nvim built from source.
PATH=$PATH:~/neovim/bin

PATH=$PATH:/bin
PATH=$PATH:/sbin
PATH=$PATH:/usr/local/bin
PATH=$HOME/homebrew/bin:$PATH
PATH=$PATH:/usr/bin
PATH=$PATH:/usr/local/sbin
PATH=$PATH:/usr/sbin
PATH=$PATH:$HOME/go/bin
