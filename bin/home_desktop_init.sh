#!/bin/bash
#
# This script is NOT idempotent! It is intended to be run
# once after installing a new operating system!

setup_xorg_conf()
{
    cd
    git clone https://github.com/brhCS/xorg.conf
    sudo mv xorg.conf/* /etc/X11
    sudo mv xorg.conf/.* /etc/X11
    sudo chown -R "$USER" /etc/X11/.git* /etc/X11/README.md
    rm -rf xorg.conf
}

setup_awesome()
{
    mkdir -p ~/.config
    git clone https://github.com/brhCS/awesome_wm ~/.config/awesome
}

create_firefox_profiles()
{
    for i in $(seq 0 5); do
        echo "Creating profile $i"
        firefox -no-remote -CreateProfile "$i"
    done
}

#setup_xorg_conf
#setup_awesome
create_firefox_profiles
