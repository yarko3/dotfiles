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
Plugin 'chazmcgarvey/vimcoder'          " Topcoder Vim Plugin
Plugin 'christoomey/vim-tmux-navigator' " Window/Pane switching with Vim and Tmux
Plugin 'dag/vim2hs'                     " Haskell error checking and syntax highlighting
Plugin 'derekwyatt/vim-fswitch'         " Fastswitch (cpp/h toggle)
Plugin 'ivanov/vim-ipython'             " Vim + IPython Notebook integration
Plugin 'justinmk/vim-syntax-extra'      " Flex and Bison syntax highlighting
Plugin 'ctrlpvim/ctrlp.vim'             " Ctrl-P
Plugin 'luochen1990/rainbow'            " Rainbow parenthesis coloring
Plugin 'majutsushi/tagbar'              " Using for JavaScript
Plugin 'scrooloose/nerdcommenter'       " Functions for easier commenting
Plugin 'scrooloose/syntastic'           " Syntax checking
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
Plugin 'kovisoft/slimv'                 " Lisp in Vim

if g:platform == "Linux"
    Plugin 'SirVer/ultisnips'               " Text snippets
    Plugin 'Valloric/YouCompleteMe'
endif

if(g:bbenv != "")
    Plugin 'https://bbgithub.dev.bloomberg.com/bhipple/bde_plugins'  " Tools for formatting code according to BDE Standards
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

" Rainbow coloring
let g:rainbow_active = 1
let g:rainbow_conf = {
\   'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
\   'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
\   'operators': '_,_',
\   'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
\   'separately': {
\       '*': {},
\       'tex': {
\           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
\       },
\       'lisp': {
\           'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
\       },
\       'vim': {
\           'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
\       },
\       'html': {
\           'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
\       },
\       'css': 0,
\   }
\}

" SLIMV
let g:slimv_repl_split=4 " Split Vertically

" Vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "35"

" YouCompleteMe
let g:ycm_server_log_level = 'debug'
let g:ycm_server_keep_logfiles = 0
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_always_populate_location_list = 1

if g:bbenv != ""
    let g:ycm_seed_identifiers_with_syntax = 1
else
    let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif
