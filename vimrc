" GVim Settings
if has("gui_running")
    set guioptions-=M    " Don't load Menu bar
    colorscheme desert
    set guifont=Monospace\ 11
else
    set t_Co=256
    colorscheme Tomorrow-Night-Eighties
endif

" Load functions
source ~/.vim/startup/functions/vimscript-helpers.vim
source ~/.vim/startup/functions/environment.vim
source ~/.vim/startup/functions/directories.vim
source ~/.vim/startup/functions/formatting.vim

" Load each specialized settings file
source ~/.vim/startup/settings.vim
source ~/.vim/startup/plugins.vim
source ~/.vim/startup/mappings.vim
