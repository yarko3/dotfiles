#!/usr/bin/env bash

update_native() {
    echo "Updating local environment..."

    if [ "$(uname)" = "Darwin" ]; then
        brew update && brew upgrade
    else
        sudo apt-get update && sudo apt-get upgrade
    fi

    echo "Finished updating local environment"
}

update_native
