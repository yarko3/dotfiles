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

setup_nvidia_drivers()
{
    sudo apt-add-repository ppa:xorg-edgers/ppa
    sudo apt-get update
    sudo apt-get install nvidia-current nvidia-settings
}

create_firefox_profiles()
{
    for i in $(seq 0 5); do
        firefox -no-remote -CreateProfile \""$i"\"
        echo "$i"
    done
}

#setup_xorg_conf
#setup_awesome
#setup_nvidia_drivers
create_firefox_profiles
