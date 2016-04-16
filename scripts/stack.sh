#!/bin/bash

if ! [ "$(which stack)" ]; then
    echo "Installing Stack"
    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    source /etc/lsb-release
    echo "Installing for Ubuntu $DISTRIB_CODENAME"
    echo "deb http://download.fpcomplete.com/ubuntu $DISTRIB_CODENAME main" | sudo tee /etc/apt/sources.list.d/fpco.list

    sudo apt-get update
    sudo apt-get install stack
else
    echo "Stack is already installed."
fi

stack setup

tools=(\
    fast-tags \
    ghc-mod \
    hdevtools \
    hindent \
    hlint \
    hoogle \
    pandoc \
    pointfree \
    regex-posix \
    turtle \
)
for t in "${tools[@]}"; do
    echo "### Stack Installing $t ###"
    stack install "$t"
done
