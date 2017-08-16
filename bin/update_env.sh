#!/usr/bin/env bash

if [ "$(uname)" = "Darwin" ]; then
    brew update && brew upgrade && brew cleanup
else
    sudo apt-get update && sudo apt-get upgrade && sudo apt-get autoremove
fi
