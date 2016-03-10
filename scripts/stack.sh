#!/bin/bash

if ! [ "$(which stack)" ]; then
    echo "Installing Stack"
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    if [ "$(lsb_release -a | grep -q '15.10')" ]; then
        echo "Installing for Ubuntu 15.10"
        echo 'deb http://download.fpcomplete.com/ubuntu wily main' | sudo tee /etc/apt/sources.list.d/fpco.list
    elif [ "$(lsb_release -a | grep -q '14.04')" ]; then
        echo "Installing for Ubuntu 14.04"
        echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | sudo tee /etc/apt/sources.list.d/fpco.list
    else
        echo "Not sure what version to add."
        echo "See: https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md#ubuntu"
        exit 0
    fi

    sudo apt-get update
    sudo apt-get install stack
else
    echo "Stack is already installed."
fi

stack setup

tools=(fast-tags hdevtools hindent hlint hoogle pointfree regex-posix)
for t in "${tools[@]}"; do
    echo "### Stack Installing $t ###"
    stack install "$t"
done
