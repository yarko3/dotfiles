export GTEST_COLOR=yes

# Command auto-correction.
export ENABLE_CORRECTION="true"


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
if [ "$TERM" != "screen-256color" ]; then
    export TERM=xterm-256color
fi

if [ "$(uname)" = "Darwin" ]; then
    export MACOS="true"
    export EDITOR=vim
fi

# Reduce delay to 0.1 seconds for switching to normal mode with ESC
export KEYTIMEOUT=20

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
