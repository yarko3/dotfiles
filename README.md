# dotfiles  [![Build Status](https://travis-ci.org/brhCS/dotfiles.svg?branch=master)](https://travis-ci.org/brhCS/dotfiles)
My dotfiles for vim, zsh, tmux, lisp, ghc, irssi, etc.

### Installation
#### Initial Install
Run this as the cloud-init user data, or on first boot:
```
#!/bin/bash
curl https://raw.githubusercontent.com/brhCS/dotfiles/master/cloud_init.sh | bash
```
#### Update and Run Idempotent Installer
```
cd ~/dotfiles
git pull
./install
```
In addition to creating symlinks with dotbot, this will run the `brh` Cookbook with `Chef Solo`.

### zgen note
To refresh and update zgen plugins, run `zgen update`. They are cached normally.

If for some reason it did not clone submodules on the first go, run `zgen reset`.
