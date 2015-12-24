#!/usr/bin/env bash
# To be run after booting a machine on a virtual cloud provider like DigitalOcean
# Put this as the user data:
# #/bin/bash
# wget https://raw.githubusercontent.com/brhCS/dotfiles/master/cloud_init.sh | bash

USR=bhipple
if ! [ -z "$1" ]; then
    USR="$1"
fi

useradd -m -s /bin/bash "$USR"
adduser "$USR" sudo

sudo -iu "$USR"
cd /home/"$USR"
mkdir .ssh
cp /root/.ssh/known_hosts /home/"$USR"/.ssh/known_hosts

touch "cloud-init-in-progress.tmp"
apt-get update
apt-get install git
git clone https://github.com/brhCS/dotfiles
cd dotfiles
./install
cd ..
git clone https://github.com/brhCS/dotfiles_local
cd dotfiles_local
./install
cd ..
rm -f "cloud-init-in-progress.tmp"
