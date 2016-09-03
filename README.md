# dotfiles [![Build Status](https://travis-ci.org/yarko3/dotfiles.svg?branch=master)](https://travis-ci.org/yarko3/dotfiles) 
My dotfiles for vim, tmux, etc.

### Installation
#### Initial Install
Run this as the cloud-init user data, or on first boot:
```
#!/bin/bash
curl https://raw.githubusercontent.com/yarko3/dotfiles/master/scripts/cloud_init.sh | bash
```
#### Update and Run Idempotent Installer
```
cd ~/dotfiles
git pull
./install
```
### License
```
Copyright © 2016 Yaroslav Senyuta <yarko3@hotmail.com>
This work is free. You can redistribute it and/or modify it under the
terms of the Do What The Fuck You Want To Public License, Version 2,
as published by Sam Hocevar. See the COPYING file for more details.
```
