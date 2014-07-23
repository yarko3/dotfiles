# ~/.profile skeleton
# ~/.profile runs on interactive login shells if it exists
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


## ============================================================================
##                           Bloomberg Environment
## ============================================================================
if [ "$BBENV" ]
then
    echo "Running Bloomberg PROFILE"

    export PATH="$PATH:/bb/bin"
    #export PATH="$PATH:/opt/bb/bin"

    # Source scraping information
    #. /bb/unsupported/scrputil/team_profile/scrpenv.sh

    # Get specific things only, so they don't mess with scrpenv
    #export PATH=/opt/bb/bin/clang:$PATH
    #export PATH=/opt/bb/bin/clang++:$PATH
    #export PATH=/opt/bb/bin/clang-3.4:$PATH
    #export PATH=/opt/bb/bin/clang-format:$PATH
    #export PATH=/opt/bb/bin/tmux:$PATH

    # My stuff
    export PATH="$PATH:$HOME/bin"

## ============================================================================
##                             Home Environment
## ============================================================================
else
    echo "Running Home PROFILE"

    PS1="${HOSTNAME}:\${PWD} \$ "
    PATH=$PATH:/usr/sbin
    ##LPDEST=put_your_printer_here
    ##GROUP=put_your_group_here
    stty erase \^\h kill \^u intr \^c
    stty echoe echok ixon ixoff -ixany

    if [ $(uname) = "SunOS" ] && [ ! "$BASH" ]
    then
        set -o emacs
        alias __A=$(print '\0020') # ^P = up = previous command
        alias __B=$(print '\0016') # ^N = down = next command
        alias __C=$(print '\0006') # ^F = right = forward a character
        alias __D=$(print '\0002') # ^B = left = back a character
        alias __H=$(print '\0001') # ^A = home = beginning of line
        stty erase ^?
        #echo "SunOS keys set"
    fi
fi


## ============================================================================
##                                  General
## ============================================================================
umask 0022

# Run BASH
if [ -x /bin/bash ]; then
    SHELL=/bin/bash
    export SHELL
    exec /bin/bash
fi

