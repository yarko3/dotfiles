" .vimrc
" Benjamin Hipple

colorscheme desert256

" === Vundle ===
" Required Vundle Configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim' " Lets Vundle manage Vundle

" Other plugins managed by Vundle

Plugin 'tpope/vim-dispatch' " Asynchronous Makes
Plugin 'tpope/vim-fugitive' " Git Wrapper
Plugin 'chazmcgarvey/vimcoder' " Topcoder Vim Plugin
Plugin 'Valloric/YouCompleteMe' " Autocomplete and CTAGS replacement
"   // Need to ask Mario about the Python libraries for the installation process

call vundle#end()
filetype plugin indent on
" === End Vundle ===


" Load each specialized settings file
source ~/.vim/startup/functions.vim
source ~/.vim/startup/mappings.vim
source ~/.vim/startup/settings.vim

let g:ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
