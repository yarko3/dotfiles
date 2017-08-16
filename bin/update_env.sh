#!/usr/bin/env bash

echo "Updating local environment..."

if [ "$(uname)" = "Darwin" ]; then
    brew update && brew upgrade && brew cleanup
else
    sudo apt-get update && sudo apt-get upgrade && sudo apt-get autoremove
fi

echo "Finished updating local environment"
