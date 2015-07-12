#!/bin/bash
#
# This script is NOT idempotent! It is intended to be run
# only once after installing a new operating system!

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

# Pre-compiled binary built from the open source project at
# https://github.com/prasmussen/gdrive
setup_google_drive_cli()
{
    wget -O drive https://drive.google.com/uc?id=0B3X9GlR6EmbnMHBMVWtKaEZXdDg
    sudo mv drive /usr/local/bin/drive
    echo "Google Drive CLI installed"
}

setup_xorg_conf
setup_awesome
create_firefox_profiles

setup_google_drive_cli
~/bin/fetch_ssh_keys.sh
