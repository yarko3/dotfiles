#!/usr/bin/env bash

update_nix() {
    if ! which nix-env 2>&1 ; then
        [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]] && . ~/.nix-profile/etc/profile.d/nix.sh
    fi
    nix-env -j 4 -u
}

update_native() {
    if [ "$(uname)" = "Darwin" ]; then
        brew update && brew upgrade
    else
        sudo apt-get update && sudo apt-get upgrade
    fi
}

echo "Updating local environment..."

update_nix
update_native

echo "Finished updating local environment"
