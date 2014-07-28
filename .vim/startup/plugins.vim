"" ============================================================================
""                                  Vundle
"" ============================================================================
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'              " Lets Vundle manage Vundle

Plugin 'Lokaltog/powerline'             " Status bar
Plugin 'Valloric/ListToggle'            " Toggling quickfix and location list
Plugin 'brhCS/bde_plugins'              " Tools for formatting code according to BDE Standards
Plugin 'chazmcgarvey/vimcoder'          " Topcoder Vim Plugin
Plugin 'chazy/cscope_maps'              " cscope mappings
Plugin 'derekwyatt/vim-fswitch'         " Fastswitch (cpp/h toggle)
Plugin 'kien/ctrlp.vim'                 " Ctrl-P
Plugin 'majutsushi/tagbar'              " Tagbar
Plugin 'scrooloose/syntastic'           " Syntax checking
Plugin 'tpope/vim-fugitive'             " Git Wrapper
Plugin 'tpope/vim-surround'             " Surrounding text
Plugin 'tpope/vim-unimpaired.git'       " Pairs of keyboard mappings for common tasks

"Plugin 'tpope/vim-dispatch'            " Asynchronous Makes
"Plugin 'kshenoy/vim-signature'         " Local marks enhancement

if g:platform == "Linux"
    Plugin 'Valloric/YouCompleteMe'
endif

call vundle#end()
filetype plugin indent on
" === End Vundle ===


"" ============================================================================
""                              Plugin Settings
"" ============================================================================
" CtrlP
let g:ctrlp_cmd = 'CtrlP ~/mbig/scrape.git'

" YouCompleteMe
let g:ycm_server_log_level = 'debug'
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_always_populate_location_list = 1

if g:bbenv != ""
    let g:ycm_path_to_python_interpreter = '/opt/swt/bin/python'
    let g:ycm_extra_conf = '~/mbig/ycm_extra_conf.py'
else
    let g:ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'
