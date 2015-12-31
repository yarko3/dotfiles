#!/bin/bash
touch "$HOME/chef-in-progress.tmp"
cd ~/.dotfiles/chef
sudo chef-client -z -o brh
rm -f "$HOME/chef-in-progress.tmp"
