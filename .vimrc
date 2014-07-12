" .vimrc
" Benjamin Hipple

" GVim Settings
if has("gui_running")
    colorscheme desert
    set guifont=Monospace\ 11
else
    colorscheme desert256
endif


" Load each specialized settings file
source ~/.vim/startup/functions.vim
source ~/.vim/startup/settings.vim
source ~/.vim/startup/plugins.vim
source ~/.vim/startup/mappings.vim
