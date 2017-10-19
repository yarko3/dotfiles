#!/usr/bin/env bash

update_nix() {
    update_nix_cmd() {
        nix-channel --update
        nix-env -j 4 -u
    }

    echo "Updating nix..."

    if ! which nix-env > /dev/null 2>&1 ; then
        if [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]]; then
            . ~/.nix-profile/etc/profile.d/nix.sh && update_nix_cmd
        else:
            echo "WARNING: nix not detected."
        fi
    else
        update_nix_cmd
    fi

    echo "Finished updating nix"
}

update_native() {
    echo "Updating local environment..."

    if [ "$(uname)" = "Darwin" ]; then
        brew update && brew upgrade
    else
        sudo apt-get update && sudo apt-get upgrade
    fi

    echo "Finished updating local environment"
}

update_nix
update_native
