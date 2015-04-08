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
Plugin 'christoomey/vim-tmux-navigator' " Window/Pane switching with Vim and Tmux
Plugin 'derekwyatt/vim-fswitch'         " Fastswitch (cpp/h toggle)
Plugin 'kien/ctrlp.vim'                 " Ctrl-P
Plugin 'majutsushi/tagbar'              " Using for JavaScript
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
"Plugin 'jeaye/color_coded'              " Clang color coding

if g:platform == "Linux"
    Plugin 'SirVer/ultisnips'               " Text snippets
    Plugin 'Valloric/YouCompleteMe'
endif

call vundle#end()
filetype plugin indent on

" === End Vundle ===
"

"" ============================================================================
""                              Plugin Settings
"" ============================================================================
" CtrlP
let g:ctrlp_working_path_mode = 'ra'

" Netrw
let g:netrw_sort_by = 'name'

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" YouCompleteMe
let g:ycm_server_log_level = 'debug'
let g:ycm_server_keep_logfiles = 0
let g:ycm_confirm_extra_conf = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_always_populate_location_list = 1

if g:bbenv != ""
    let g:ycm_seed_identifiers_with_syntax = 1
else
    let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif
