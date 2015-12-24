#!/usr/bin/env bash
# To be run after booting a machine on a virtual cloud provider like DigitalOcean
#
# Put this as the user data:
#!/bin/bash
#curl https://raw.githubusercontent.com/brhCS/dotfiles/master/cloud_init.sh | bash
#
# Inspect its progress with:
# less +F /var/log/cloud-init-output.log

USR=bhipple
if ! [ -z "$1" ]; then
    USR="$1"
fi

echo "Starting dotfiles cloud_init.sh with USR=$USR"

apt-get update
apt-get install -y git zsh

useradd -m -s /usr/bin/zsh "$USR"
adduser "$USR" sudo

export HOME=/home/$USR
export USER=$USR
cd /home/"$USR"
touch "cloud-init-in-progress.tmp"

mkdir .ssh
cp /root/.ssh/authorized_keys /home/"$USR"/.ssh/authorized_keys

echo "Setting up dotfiles"
git clone https://github.com/brhCS/dotfiles
cd dotfiles
./install

cd /home/"$USR"
rm -f "cloud-init-in-progress.tmp"

echo "Changing ownership from root to $USR"
chown "$USR:$USR" -R .

echo "Finished dotfiles cloud_init.sh"
