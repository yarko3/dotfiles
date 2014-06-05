" Vim Plugins


" === Vundle ===
" Required Vundle Configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'          " Lets Vundle manage Vundle
"Plugin 'tpope/vim-dispatch'         " Asynchronous Makes
Plugin 'tpope/vim-fugitive'         " Git Wrapper
Plugin 'chazmcgarvey/vimcoder'      " Topcoder Vim Plugin
Plugin 'kien/ctrlp.vim'             " Ctrl-P
Plugin 'majutsushi/tagbar'          " Tagbar
Plugin 'derekwyatt/vim-fswitch'     " Fastswitch (cpp/h toggle)

" Linux-Only plug-ins
if g:platform == "Linux"
    Plugin 'Valloric/YouCompleteMe'
    "let g:ycm_server_log_level = 'debug'
    let g:ycm_confirm_extra_conf = 0
    let g:ycm_path_to_python_interpreter = '/opt/swt/bin/python'
    let g:ycm_autoclose_preview_window_after_insertion = 1
    let g:ycm_always_populate_location_list = 1
    let g:ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif

call vundle#end()
filetype plugin indent on
" === End Vundle ===

