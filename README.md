#dotfiles
My dotfiles for vim, zsh, tmux, etc.

##Installation from a fresh OS install on my home desktop or laptop
```
sudo apt-get install -y git
git clone https://github.com/brhCS/dotfiles
cd dotfiles && ./install
bin/install_programs.sh -i
chsh -s $(which zsh)
```
If using local dotfiles, `cd ~/dotfiles_local && ./install`

Then copy my private ssh key from Google Drive into `~/.ssh/brh-key`

### zgen note
To refresh and update zgen plugins, run `zgen update`. They are cached
normally.
