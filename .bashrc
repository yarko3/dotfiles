#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return

## ============================================================================
##                           Bloomberg Environment
## ============================================================================
if [ "$BBENV" ]; then
    echo "Running Bloomberg BASHRC"
    umask 0022

    # if chimera generated aliases exist, pull them into the current ENV
    [ -f ~/.bbalias ] && . ~/.bbalias

    # Authentication
    alias rf='vastool kinit -R && vastool klist'
    alias make='vastool kinit -R && make'
    alias git='vastool kinit -R && git'

    # Autocomplete for Git
    GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
    [ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

    # Programs
    alias scrptogui='rm -rf scraper.0.log.* && ./run.sh testmsg.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'
    alias scrpgui='../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'
    alias cfd='clang-format -style=file -i *.cpp *.h'

    # Scripts
    alias cs='~/scr/cd_scraper.sh'
    alias pw='~/scr/pwhat.sh'

    # cd
    alias cdbbit='cd /bbsrc/thirdparty/bbit/include'
    alias cdbig='cd ~/mbig/'
    alias cdgit='cd ~/mbig/scrape.git'

    alias cdscrpxbsvc='cd ~/mbig/scrape.git/msgscrape/pcs_xb_mapping/scrpxbsvc/'


## ============================================================================
##                             Home Environment
## ============================================================================
else
    echo "Running Home BASHRC"
    umask 0022

    export PATH="$PATH:~/bin"

    if [ $HOSTNAME == 'brh-laptop' ]; then
        xmodmap ~/.Xmodmaps/Xmodmap_kubuntu_laptop
    elif [ $HOSTNAME == 'brh-desktop' ]; then
        xmodmap ~/.Xmodmaps/Xmodmap_kubuntu_desktop
    fi

    # Programs
    alias ff='~/scr/x_server/firefox_launch.sh &'
    alias firefox='~/scr/x_server/firefox_launch.sh &'
    alias gc='~/scr/x_server/chrome_launch.sh &'
    alias google-chrome='~/scr/x_server/chrome_launch.sh &'
fi


## ============================================================================
##                                  General
## ============================================================================
export TERM=xterm-256color
export EDITOR=vim

# update winsize after each command for better line-wrapping
shopt -s  checkwinsize

alias vi='vim'
alias gvir='gvim --remote-send ":tabe<CR>" && gvim --remote'
alias g='git'
alias ls='ls --color'

# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '
