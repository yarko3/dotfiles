"" ============================================================================
""                                  Vundle
"" ============================================================================
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'              " Lets Vundle manage Vundle

Plugin 'Valloric/ListToggle'            " Toggling quickfix and location list
Plugin 'benmills/vimux'                 " Vim and Tmux Integration
Plugin 'bling/vim-airline'              " Status line
Plugin 'brhCS/bde_plugins'              " Tools for formatting code according to BDE Standards
Plugin 'chazmcgarvey/vimcoder'          " Topcoder Vim Plugin
Plugin 'chazy/cscope_maps'              " cscope mappings
Plugin 'christoomey/vim-tmux-navigator' " Window/Pane switching with Vim and Tmux
Plugin 'derekwyatt/vim-fswitch'         " Fastswitch (cpp/h toggle)
Plugin 'kien/ctrlp.vim'                 " Ctrl-P
Plugin 'tpope/vim-abolish'              " Coercion and Subvert
Plugin 'tpope/vim-commentary'           " Comment/uncomment operator
Plugin 'tpope/vim-dispatch'             " Asynchronous Makes
Plugin 'tpope/vim-fugitive'             " Git Wrapper
Plugin 'tpope/vim-git'                  " Setting textwidth on git commits
Plugin 'tpope/vim-repeat'               " Dot operator for plugins
Plugin 'tpope/vim-surround'             " Surrounding text
Plugin 'tpope/vim-unimpaired.git'       " Pairs of keyboard mappings for common tasks
Plugin 'tpope/vim-vinegar.git'          " netrw improvement
Plugin 'vim-scripts/Tabmerge'           " Merge tabs into splits

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

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" Syntastic
let g:syntasti_ignore_files = ['cpp$', 'h']
let g:syntastic_cpp_check_header = 0
let g:syntastic_cpp_remove_include_errors = 1

" YouCompleteMe
let g:ycm_server_log_level = 'debug'
let g:ycm_server_keep_logfiles = 0
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_always_populate_location_list = 1

if g:bbenv != ""
    "let g:ycm_filetype_specific_completion_to_disable = { 'c': 0, 'cpp': 0 }
    let g:ycm_path_to_python_interpreter = '/opt/swt/bin/python'
    let g:ycm_global_ycm_extra_conf = '~/mbig/.ycm_extra_conf.py'
else
    let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif
