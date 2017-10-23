# dotfiles [![Build Status](https://travis-ci.org/yarko3/dotfiles.svg?branch=master)](https://travis-ci.org/yarko3/dotfiles)
My dotfiles for vim, tmux, etc.

### Installation
```
cd ~/dotfiles
git pull
./install
```

### Caveats
1. Default [push] option in the gitconfig is not supported by earlier versions of git.
2. After initial run of ./install, the $PATH will not be populated with nixpkgs; start a new shell.
