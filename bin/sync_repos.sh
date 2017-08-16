#!/usr/bin/env bash

check_uncommitted() {
    diff=$(git diff-index HEAD)
    if [ -n "$diff" ]; then
        echo "WARNING: $1 has uncommitted files."
    fi
}

update() {
    f() {
        echo "Synchronizing $1"
        cd "$1" || exit 1
        check_uncommitted "$1"
        git pull > /dev/null
        git push > /dev/null
    }
    if [ -d "$1" ]; then
        f "$1" &
    else
        echo "Skipping $1 - Not deployed on this machine."
    fi
}


update ~/.dotfiles
update ~/.dotfiles_local

wait
echo "Finished updating"
