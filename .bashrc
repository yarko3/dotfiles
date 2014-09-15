#!/bin/bash
## don't read bashrc if session is not interactive
[ -z "$PS1" ] && return

## ============================================================================
##                           Bloomberg Environment
## ============================================================================
if [ "$BBENV" ]; then
    echo "Running Bloomberg BASHRC"
    umask 0022

    #xmodmap ~/.Xmodmaps/Xmodmap_bloomberg

    # if chimera generated aliases exist, pull them into the current ENV
    [ -f ~/.bbalias ] && . ~/.bbalias

    # Authentication
    alias rf='vastool kinit -R && vastool klist'
    alias make='vastool kinit -R && make'

    # Autocomplete for Git
    GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
    [ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

    # Programs
    alias scrptogui='rm -rf scraper.0.log.* && ./run.sh testmsg.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'
    alias scrpgui='../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'
    alias cfd='clang-format -style=file -i *.cpp *.h'

    # cd
    alias cdbbit='cd /bbsrc/thirdparty/bbit/include'
    alias cdbig='cd ~/mbig/'
    alias cdgit='cd ~/mbig/scrape.git'

    # cd msgscrape subdirectories
    alias cdide='cd ~/mbig/scrape.git/msgscrape/sxide/'
    alias cdmtm='cd ~/mbig/scrape.git/msgscrape/mtm/'
    alias cdscqtodb='cd ~/mbig/scrape.git/msgscrape/scqtodb/'
    alias cdscqtosvc='cd ~/mbig/scrape.git/msgscrape/scqtosvc/'
    alias cdscqtrsvc='cd ~/mbig/scrape.git/msgscrape/scqtrsvc/'
    alias cdscradsvc='cd ~/mbig/scrape.git/msgscrape/scradsvc/'
    alias cdscrapefe='cd ~/mbig/scrape.git/msgscrape/scrapefe/'
    alias cdscraper='cd ~/mbig/scrape.git/msgscrape/scraper/'
    alias cdscrpenab='cd ~/mbig/scrape.git/msgscrape/scrpenab/'
    alias cdscrpfe='cd ~/mbig/scrape.git/msgscrape/scrapefe/'
    alias cdscrpfebr='cd ~/mbig/scrape.git/msgscrape/scrapefe/scrpfebr/'
    alias cdscrpfxib='cd ~/mbig/scrape.git/msgscrape/scrapefe/scrpfxib/'
    alias cdscrpxbsvc='cd ~/mbig/scrape.git/msgscrape/pcs_xb_mapping/scrpxbsvc/'
    alias cdsxtmpsvc='cd ~/mbig/scrape.git/msgscrape/s_sxtmpsvc/'
    alias cdsxxbsvc='cd ~/mbig/scrape.git/msgscrape/sxxbsvc/'

    # pwhats
    alias pwhatscqtosvc='pwhat /scrp/bin/scqtosvc.tsk'
    alias pwhatscrpxbsvc='pwhat /scrp/bin/scrpxbsvc.tsk'
    alias pwhatsxxbsvc='pwhat /scrp/bin/sxxbsvc.tsk'


## ============================================================================
##                             Home Environment
## ============================================================================
else
    echo "Running Home BASHRC"
    umask 0022

    if [ $HOSTNAME == 'brh-laptop' ]; then
        xmodmap ~/.Xmodmaps/Xmodmap_kubuntu_laptop
    elif [ $HOSTNAME == 'brh-desktop' ]; then
        xmodmap ~/.Xmodmaps/Xmodmap_kubuntu_desktop
    fi

    # Programs
    alias ff='nohup ~/scr/x_server/firefox_launch.sh &'
    alias firefox='nohup ~/scr/x_server/firefox_launch.sh &'
    alias gc='nohup ~/scr/x_server/chrome_launch.sh &'
    alias google-chrome='nohup ~/scr/x_server/chrome_launch.sh &'
fi


## ============================================================================
##                                  General
## ============================================================================
export TERM=xterm-256color
export EDITOR=vim

# update winsize after each command for better line-wrapping
shopt -s  checkwinsize

alias gvir='gvim --remote-send ":tabe<CR>" && gvim --remote'
alias g='git'
alias ls='ls --color'

# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '
