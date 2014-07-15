#!/bin/bash

# ~/.bashrc skeleton
# ~/.bashrc runs ONLY on non-login subshells! (different from ksh)
# add lines here very carefully as this may execute when you don't
# expect them to
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# if chimera generated aliases exist, pull them into the current ENV
[ -f ~/.bbalias ] && . ~/.bbalias

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# bash >= 3.x shell options
# update winsize after each command for better line-wrapping
shopt -s  checkwinsize

# Default file creation permissions
if [ "$BBENV" ]; then
    echo "Running Bloomberg BASHRC"
    umask a-rwx
    umask u+wrx,g+rw,o+r
else
    echo "Running Home BASHRC"
    umask 0022
fi


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Settings
#
# Bloomberg Configurations
if [ "$BBENV" ]
then
    xmodmap ~/.Xmodmaps/Xmodmap_bloomberg

    # Autocomplete for Git
    GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
    [ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

    # Programs
    alias scrptogui='rm -rf scraper.0.log.* && ./run.sh testmsg.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'
    alias scrpgui='../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'

    # cd
    alias cdbbit='cd /bbsrc/thirdparty/bbit/include'
    alias cdgit='cd ~/mbig/scrape.git'
    alias cdide='cd ~/mbig/scrape.git/msgscrape/sxide/'
    alias cdmtm='cd ~/mbig/scrape.git/msgscrape/mtm/mtmcore/'
    alias cdscraper='cd ~/mbig/scrape.git/msgscrape/scraper/'
    alias cdtest='cd /scrp/data/scraper/testdata/test'

    # cd to bas services
    alias cdscqtosvc='cd ~/mbig/scrape.git/msgscrape/scqtosvc/'
    alias cdscqtrsvc='cd ~/mbig/scrape.git/msgscrape/scqtrsvc/'
    alias cdscradsvc='cd ~/mbig/scrape.git/msgscrape/scradsvc/'
    alias cdscrpxbsvc='cd ~/mbig/scrape.git/msgscrape/pcs_xb_mapping/scrpxbsvc/'
    alias cdsxtmpsvc='cd ~/mbig/scrape.git/msgscrape/s_sxtmpsvc/'
    alias cdsxxbsvc='cd ~/mbig/scrape.git/msgscrape/sxxbsvc/'

# Personal Configurations
else
    if [ $HOSTNAME == 'brh-laptop' ]; then
        xmodmap ~/.Xmodmaps/Xmodmap_kubuntu_laptop
    elif [ $HOSTNAME == 'brh-desktop' ]; then
        xmodmap ~/.Xmodmaps/Xmodmap_kubuntu_desktop
    fi

    # Programs
    alias ff='nohup ~/.x_server/firefox_launch.sh &'
    alias firefox='nohup ~/.x_server/firefox_launch.sh &'
    alias gc='nohup ~/.x_server/chrome_launch.sh &'
    alias google-chrome='nohup ~/.x_server/chrome_launch.sh &'
fi

# Platform independent settings
alias gvir='gvim --remote-send ":tabe<CR>" && gvim --remote'

export TERM=xterm-256color
export EDITOR=vim


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Aliases
alias vi='vim'

# Directory coloring
alias ls='ls --color'
