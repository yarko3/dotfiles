cd
ln -fs /home/bhipple/dotfiles_local /home/bhipple/.dotfiles_local
cd -

PATH=~/.dotfiles/bin:~/.dotfiles_local/bin_local:${PATH}
