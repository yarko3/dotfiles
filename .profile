# ~/.profile skeleton
# ~/.profile runs on interactive login shells if it exists
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
echo "PROFILE has run for user $USER and BBENV = $BBENV ."

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# chimera not present/didn't run, set some basic stuff up
# hope /etc/passwd is good enough
if ["$USER" != "brh"]; then
if [!"$BBENV" ]; then
     echo "Setting in up basic config with profile instead of chimera"
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
    umask 0022
fi
fi

if [ "$BBENV" ]
then
    # Include this source for scraping information
    . /bb/unsupported/scrputil/team_profile/scrpenv.sh

    export PATH="$PATH:/bb/bin"
fi


if [ !"$BBENV" ]; then
    if [ -n "$BASH_VERSION" ]; then
        if [ -f "$HOME/.bashrc" ]; then
            . "$HOME/.bashrc"
        fi
    fi

    if [ -d "$HOME/bin" ] ; then
        PATH="$HOME/bin:$PATH"
    fi
fi

# Run BASH
if [ -x /bin/bash ]; then
    SHELL=/bin/bash
    export SHELL
    exec /bin/bash
fi

