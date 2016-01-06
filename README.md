# dotfiles  [![Build Status](https://travis-ci.org/brhCS/dotfiles.svg?branch=master)](https://travis-ci.org/brhCS/dotfiles)
My dotfiles for vim, zsh, tmux, lisp, ghc, irssi, etc.

### Installation
#### Initial Install
Run this as the cloud-init user data, or on first boot:
```
#!/bin/bash
curl https://raw.githubusercontent.com/brhCS/dotfiles/master/shell/cloud_init.sh | bash
```
#### Update and Run Idempotent Installer
```
cd ~/dotfiles
git pull
./install
```
In addition to creating symlinks with dotbot, this will run the `brh` Cookbook with `Chef Solo`.

### License
```
Copyright © 2015 Benjamin Hipple <benjamin.hipple@gmail.com>
This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To Public License, Version 2,
as published by Sam Hocevar. See the COPYING file for more details.
```
