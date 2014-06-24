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
    umask a-rwx
    umask u+wrx,g+rw,o+r
else
    umask 0022
fi


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Settings
#
# Bloomberg Configurations
if [ "$BBENV" ]
then
    xmodmap ~/.Xmodmap_bloomberg

    # Autocomplete for Git
    GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
    [ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

    # Programs
    alias scrptogui='rm -rf scraper.0.log.* && ./run.sh testmsg.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'
    alias scrpgui='../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg'

    alias scrptest='rm -rf scraper.0.log.* && ./run.sh testmsg.brh.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.brh.cfg &'

    # cd
    alias cdgit='cd ~/mbig/scrape.git'
    alias cdide='cd ~/mbig/scrape.git/msgscrape/sxide/'
    alias cdmtm='cd ~/mbig/scrape.git/msgscrape/mtm/mtmcore/'
    alias cdscradsvc='cd ~/mbig/scrape.git/msgscrape/scradsvc/'
    alias cdscraper='cd ~/mbig/scrape.git/msgscrape/scraper/'
    alias cdsxtmpsvc='cd ~/mbig/scrape.git/msgscrape/s_sxtmpsvc/'
    alias cdtest='cd /scrp/data/scraper/testdata/test'


# Personal Configurations
else
    if [ $HOSTNAME == 'brh-laptop' ]; then
        xmodmap ~/.Xmodmap_kubuntu_laptop
    elif [ $HOSTNAME == 'brh-desktop' ]; then
        xmodmap ~/.Xmodmap_kubuntu_desktop
    fi

    # Programs
    alias ff='nohup ~/.firefox-x-launch.sh &'
    alias firefox='nohup ~/.firefox-x-launch.sh &'
    alias gc='nohup ~/.chrome-x-launch.sh'
    alias google-chrome='nohup ~/.chrome-x-launch.sh'
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

echo "BASHRC has run"
