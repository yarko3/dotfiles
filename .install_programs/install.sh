#!/bin/bash
# Install script for setting up all of my programs on Linux

## ============================================================================
##                                  Sources
## ============================================================================
# See my_sources.list


## ============================================================================
##                                 Functions
## ============================================================================
function printHelp() {
    echo "Ben's Software Manager"
    echo "   Options:"
    echo "   -h  Print Help"
    echo "   -u  Update and Upgrade"
    echo "   -i  Install new programs"
}

function update() {
    sudo apt-get -y update
}

function upgrade() {
    sudo apt-get -y upgrade
}

function installAll() {
    # Development
    sudo apt-get -y install build-essential cmake
    sudo apt-get -y install python-dev
    sudo apt-get -y build-dep python3.4
    sudo apt-get -y install icedtea-netx
    sudo apt-get -y install subversion
    sudo apt-get -y install python-software-properties pkg-config
    sudo apt-get -y install software-properties-common

    # Citrix
    #sudo apt-get -y install libmotif4:i386 nspluginwrapper lib32z1 libc6-i386 libxp6:i386 libxpm4:i386 libasound2:i386

    # Browsers
    sudo apt-get -y install chromium-browser
    sudo apt-get -y install google-chrome
    sudo apt-get -y install flashplugin-installer

    # Other
    sudo apt-get -y install ubuntu-restricted-extras
    sudo apt-get -y install filezilla
    sudo apt-get -y install keepass2
    sudo apt-get -y install skype
    sudo apt-get -y install wireshark
    sudo apt-get -y install deluge

    # Media Players
    sudo apt-get -y install totem
    sudo apt-get -y install xbmc
}


## ============================================================================
##                                  OptArgs
## ============================================================================
if [ -z "$1" ]; then printHelp; fi

while getopts "hui" opt; do
    case $opt in
        h)
            printHelp
            ;;
        u)
            echo "Updating and Upgrading"
            update
            upgrade
            ;;
        i)
            echo "Installing programs" >&2
            installAll
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            exit 1
            ;;
        :)
            echo "Option -$OPTARG requires an argument." >&2
            exit 1
            ;;
    esac
done
