#!/usr/bin/env bash

update_native() {
    echo "Updating local environment..."

    if [ "$(uname)" = "Linux" ]; then
        sudo apt-get update && sudo apt-get upgrade
    else
        echo "No local environment to set up..."
    fi

    echo "Finished updating local environment"
}

update_native
