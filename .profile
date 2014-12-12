# ~/.profile skeleton
# ~/.profile runs on interactive login shells if it exists
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=


## ============================================================================
##                           Bloomberg Environment
## ============================================================================
if [ "$BBENV" ]; then
    echo "Running Bloomberg PROFILE"

    export PATH="$PATH:/bb/bin"
    export PATH="$PATH:/opt/bb/bin"
    export PATH="$PATH:/bb/bigstorq4/scrpbuild/devtools/bin"

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

    export PATH=$PATH:/opt/swt/common/cov-analysis-linux64-7.5.1/bin

    # My stuff
    export PATH="$HOME/bin:$PATH"

    # Run BASH
    if [ -x /bin/bash ]; then
        SHELL=/bin/bash
        export SHELL
        exec /bin/bash
    fi
fi
## ============================================================================
##                             Home Environment
## ============================================================================
# Currently nothing to do

## ============================================================================
##                                  General
## ============================================================================
umask 0022
