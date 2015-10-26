#!/bin/bash
##########################################
# Install the Chef Developer Kit
#
if [ -z "$(which chef-apply)" ]; then
    echo "Installing Chef DK"
    wget https://opscode-omnibus-packages.s3.amazonaws.com/debian/6/x86_64/chefdk_0.9.0-1_amd64.deb
    sudo dpkg -i chefdk_0.9.0-1_amd64.deb
    rm chefdk_0.9.0-1_amd64.deb
else
    echo "Chef DK already installed"
fi

##########################################
# Change shell to zsh, if not already done
#
if [ $(echo "$SHELL" | grep -c "zsh") -eq "0" ]; then
    echo "Setting shell to zsh"
    chsh -s $(which zsh)
else
    echo "zsh is already the default shell"
fi

#############################################
# Create ssh dir with appropriate permissions
#
mkdir -p $HOME/.ssh
chmod 0700 $HOME/.ssh
