#!/usr/bin/env bash

has_uncommitted_changes() {
    diff=$(git diff-index HEAD)
    if [ -n "$diff" ]; then
        echo "WARNING: $1 has uncommitted files."
        return 0
    fi
    return 1
}

check_submodules_and_commit_on_update() {
    git submodule update --remote --init
    if has_uncommitted_changes "$1"; then
        git add -u
        git commit -m "updating submodules"
    fi
}

update() {
    f() {
        echo "Synchronizing $1"
        cd "$1" || exit 1
        if ! has_uncommitted_changes "$1"; then
            git pull
            check_submodules_and_commit_on_update "$1"
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
