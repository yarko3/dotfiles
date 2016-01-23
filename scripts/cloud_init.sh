#!/usr/bin/env bash
# To be run after booting a machine on a virtual cloud provider like DigitalOcean
#
# Inspect its progress with:
# less +F /var/log/cloud-init-output.log
USR=bhipple
if ! [ -z "$1" ]; then
    USR="$1"
fi

echo "Starting dotfiles cloud_init.sh with USR=$USR"

apt-get update
apt-get install -y git zsh build-essential cmake python-dev ghc

useradd -m -s /usr/bin/zsh "$USR"
adduser "$USR" sudo

export HOME=/home/$USR
export USER=$USR
cd /home/"$USR"

mkdir .ssh
cp /root/.ssh/authorized_keys /home/"$USR"/.ssh/authorized_keys
chown -R "$USR:$USR" /home/"$USR"/.ssh

echo "Setting up dotfiles"
git clone https://github.com/brhCS/dotfiles
cd dotfiles
./install

echo "Finished dotfiles cloud_init.sh"
