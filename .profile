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
    export PATH="$PATH:/opt/bb/bin"

    # Setup scraping team profile
    [ -f /bb/unsupported/scrputil/team_profile/scrpenv.sh ] && . /bb/unsupported/scrputil/team_profile/scrpenv.sh

    # Source scraping environment variables
    [ -f /scrp/bin/scraper_instances.sh ] && . /scrp/bin/scraper_instances.sh

    # db2
    [ -f /bb/db2/home/db2c/sqllib/db2profile ] && . /bb/db2/home/db2c/sqllib/db2profile

    # Get specific things only, so they don't mess with scrpenv
    export PATH=/opt/bb/bin/clang:$PATH
    export PATH=/opt/bb/bin/clang-cl:$PATH
    export PATH=/opt/bb/bin/clang++:$PATH
    export PATH=/opt/bb/bin/clang-format:$PATH
    export PATH=/opt/bb/bin/clang-check:$PATH
    export PATH=/opt/bb/bin/git-clang-format:$PATH
    export PATH=/opt/bb/bin/tmux:$PATH

    # My stuff
    export PATH="$PATH:$HOME/bin"

    # Run BASH
    if [ -x /bin/bash ]; then
        SHELL=/bin/bash
        export SHELL
        exec /bin/bash
    fi
## ============================================================================
##                             Home Environment
## ============================================================================
else
    echo "Running Home PROFILE"
    #PATH=$PATH:/usr/sbin
fi

## ============================================================================
##                                  General
## ============================================================================
umask 0022
