#!/usr/bin/env bash
# To be run after booting a machine on a virtual cloud provider like DigitalOcean
# Put this as the user data:
# #!/bin/bash
# curl https://raw.githubusercontent.com/brhCS/dotfiles/master/cloud_init.sh | bash

USR=bhipple
if ! [ -z "$1" ]; then
    USR="$1"
fi

useradd -m -s /bin/bash "$USR"
adduser "$USR" sudo

export HOME=/home/$USR
export USER=$USR
cd /home/"$USR"
mkdir .ssh
cp /root/.ssh/authorized_keys /home/"$USR"/.ssh/authorized_keys

touch "cloud-init-in-progress.tmp"
apt-get update
apt-get install -y git
git clone https://github.com/brhCS/dotfiles
cd dotfiles
./install
cd ..
git clone https://github.com/brhCS/dotfiles_local
cd dotfiles_local
./install
cd ..
rm -f "cloud-init-in-progress.tmp"

chown "$USR:$USR" -R .
