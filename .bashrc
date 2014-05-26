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


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Settings
if [ "$BBENV"]
then
    xmodmap ~/.Xmodmap_bloomberg
else
    xmodmap ~/.Xmodmap_kubuntu
fi

export TERM=xterm-256color
export EDITOR=vim

# Autocomplete for Git
GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
[ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Prompt
PS1='\[\e[1;31m\][\u@\h: \w]\$\[\e[0m\] '

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Aliases
alias vi='vim'

# Directory coloring
alias ls='ls --color'

# Programs
alias scrapetogui='cd ~/mbig/scrape.git/msgscrape/scraper/ && ./run.sh testmsg.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg &'
alias scrapegui='cd ~/mbig/scrape.git/msgscrape/scraper/ && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg &'

# cd
alias cdscraper='cd ~/mbig/scrape.git/msgscrape/scraper/'
alias cdmtm='cd ~/mbig/scrape.git/msgscrape/mtm/mtmcore/'
alias cdgit='cd ~/mbig/scrape.git'

alias cdscradsvc='cd ~/mbig/scrape.git/msgscrape/scradsvc/'
alias cdsxtmpsvc='cd ~/mbig/scrape.git/msgscrape/s_sxtmpsvc/'


echo "BASHRC has run"
