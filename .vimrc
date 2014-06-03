" .vimrc
" Benjamin Hipple

if has("gui_running")
    colorscheme desert
else
    colorscheme desert256
endif


" Load each specialized settings file
source ~/.vim/startup/functions.vim
source ~/.vim/startup/settings.vim
source ~/.vim/startup/mappings.vim
source ~/.vim/startup/plugins.vim
