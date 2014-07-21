" Vim Plugins


" === Vundle ===
" Required Vundle Configuration
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'              " Lets Vundle manage Vundle

Plugin 'Valloric/ListToggle'            " Toggling quickfix and location list
Plugin 'brhCS/bde_plugins'              " Tools for formatting code according to BDE Standards
Plugin 'chazmcgarvey/vimcoder'          " Topcoder Vim Plugin
Plugin 'chazy/cscope_maps'              " cscope mappings
Plugin 'derekwyatt/vim-fswitch'         " Fastswitch (cpp/h toggle)
Plugin 'kien/ctrlp.vim'                 " Ctrl-P
Plugin 'majutsushi/tagbar'              " Tagbar
Plugin 'tpope/vim-fugitive'             " Git Wrapper
Plugin 'tpope/vim-surround'             " Surrounding text
Plugin 'tpope/vim-unimpaired.git'       " Pairs of keyboard mappings for common tasks

"Plugin 'tpope/vim-dispatch'            " Asynchronous Makes
"Plugin 'kshenoy/vim-signature'         " Local marks enhancement

" Linux-Only plug-ins
if g:platform == "Linux"
    Plugin 'Valloric/YouCompleteMe'
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
endif

call vundle#end()
filetype plugin indent on
" === End Vundle ===

