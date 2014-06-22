#!/bin/bash
# Install script for setting up all of my programs on Linux

#==============================================================================
# Adding Repos
#
# See my_sources.list

#==============================================================================
# Update
#
sudo apt-get update

#==============================================================================
# Specific Installs
#
# Development
sudo apt-get install build-essential cmake
sudo apt-get install python-dev
sudo apt-get build-dep python3.4

# Citrix
sudo apt-get install libmotif4:i386 nspluginwrapper lib32z1 libc6-i386 libxp6:i386 libxpm4:i386 libasound2:i386

# Browsers
sudo apt-get install chromium-browser
sudo apt-get install google-chrome
sudo apt-get install flashplugin-installer


# Other
sudo apt-get install filezilla
sudo apt-get install keepass2
sudo apt-get install skype
sudo apt-get install wireshark

#==============================================================================
# Generic Upgrade
#
sudo apt-get upgrade
