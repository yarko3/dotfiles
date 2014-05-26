#!/bin/bash


# Adding Repos
sudo apt-add-repository ppa:jtaylor/keepass
sudo add-apt-repository "deb http://archive.canonical.com/ $(lsb_release -sc) partner"

# Upddate
sudo apt-get update

# Specific Installs
sudo apt-get install keepass2
sudo apt-get install skype

# Generic Upgrade
sudo apt-get upgrade
