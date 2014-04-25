" .vimrc
" Benjamin Hipple

colorscheme desert256

" Pathogen startup
execute pathogen#infect()

" Load each specialized settings file
source ~/.vim/startup/mappings.vim
source ~/.vim/startup/settings.vim
