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
umask u+wrx,g+rw,o+rw

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Settings
if [ "$BBENV" ]
then
    xmodmap ~/.Xmodmap_bloomberg

    # Autocomplete for Git
    GIT_AUTOCOMPLETE_PATH="/opt/swt/share/git-contrib/completion/git-completion.bash"
    [ -f $GIT_AUTOCOMPLETE_PATH ] && source $GIT_AUTOCOMPLETE_PATH

    # Programs
    alias scrapetogui='cd ~/mbig/scrape.git/msgscrape/scraper/ && rm -rf scraper.0.log.* && ./run.sh testmsg.msg && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg &'
    alias scrapegui='cd ~/mbig/scrape.git/msgscrape/scraper/ && ../sxide/demos/reverse-pattern-lookup.py ./scraper_dump.tcl ./patterns.cfg &'

    # cd
    alias cdgit='cd ~/mbig/scrape.git'
    alias cdide='cd ~/mbig/scrape.git/msgscrape/sxide/'
    alias cdmtm='cd ~/mbig/scrape.git/msgscrape/mtm/mtmcore/'
    alias cdscradsvc='cd ~/mbig/scrape.git/msgscrape/scradsvc/'
    alias cdscraper='cd ~/mbig/scrape.git/msgscrape/scraper/'
    alias cdsxtmpsvc='cd ~/mbig/scrape.git/msgscrape/s_sxtmpsvc/'

else
    xmodmap ~/.Xmodmap_kubuntu

    # Programs
    alias ff='firefox &'
    alias gc='google-chrome &'
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
