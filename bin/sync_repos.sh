#!/usr/bin/env bash

check_uncommitted() {
    diff=$(git diff-index HEAD)
    if [ -n "$diff" ]; then
        echo "WARNING: $1 has uncommitted files."
        return 0
    fi
    return 1
}

update() {
    f() {
        echo "Synchronizing $1"
        cd "$1" || exit 1
        if ! check_uncommitted "$1"; then
            git pull

            git submodule update --remote --init
            if ! check_uncommitted "$1"; then
                git add -u
                git commit -m "updating submodules"
            fi

            git push
        fi
    }
    if [ -d "$1" ]; then
        f "$1"
    else
        echo "Skipping $1 - Not deployed on this machine."
    fi
}

echo "Updating repos"

update ~/.dotfiles
update ~/.dotfiles_local

wait
echo "Finished updating repos"
