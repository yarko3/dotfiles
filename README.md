##dotfiles
My dotfiles for vim, zsh, tmux, etc.

###Basic Installation from a fresh OS install
```
sudo apt-get install -y git
git clone https://github.com/brhCS/dotfiles
cd dotfiles && ./install
bin/install_programs.sh -il
chsh -s $(which zsh)
```
If using local dotfiles, `cd ~/dotfiles_local && ./install`

Then copy my private ssh keys from Google Drive into `~/.ssh/brh-key` and `~/.ssh/id_rsa`

###Extra notes for my home desktop
After doing the above, also:
 * Run `bin/home_desktop_init.sh` (see script for details)
 * Install and use nVidia proprietary gfx drivers, directly from nVidia's website

### zgen note
To refresh and update zgen plugins, run `zgen update`. They are cached normally.  If for some reason it did not clone submodules on the first go, run `zgen reset`.
