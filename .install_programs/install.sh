#!/bin/bash
# Install script for setting up all of my programs on Linux
#
# Command Line Arguments
#   -a     Installs all programs, instead of simply updating and upgrading

while getopts "a" opt; do
  case $opt in
    a)
      echo "Installing all programs" >&2
      INSTALL_ALL=true
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

#==============================================================================
# Adding Repos
#
# See my_sources.list

#==============================================================================
# Update
#
sudo apt-get -y update

#==============================================================================
# Specific Installs
if [ $INSTALL_ALL ]; then
    #
    # Development
    sudo apt-get -y install build-essential cmake
    sudo apt-get -y install python-dev
    sudo apt-get -y build-dep python3.4

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
fi

#==============================================================================
# Generic Upgrade
#
sudo apt-get -y upgrade
