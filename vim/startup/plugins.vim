call plug#begin('~/.vim/plugged')

Plug 'Shougo/vimproc', { 'do': 'make' }                    " Asynchronous command execution library
Plug 'Valloric/ListToggle'                                 " Toggling quickfix and location list
Plug 'bling/vim-airline'                                   " Status line
Plug 'chazmcgarvey/vimcoder'                               " Topcoder Vim Plugin
Plug 'christoomey/vim-tmux-navigator'                      " Window/Pane switching with Vim and Tmux
Plug 'ctrlpvim/ctrlp.vim'                                  " File searchin and opening
Plug 'derekwyatt/vim-fswitch', { 'for': 'cpp' }            " Fastswitch (cpp/h toggle)
Plug 'ivanov/vim-ipython', { 'for': 'python' }             " Vim + IPython Notebook integration
Plug 'jceb/vim-orgmode'                                    " Emacs orgmode port
Plug 'mhinz/vim-grepper'                                   " Asynchronous Grep -> QuickFix List
Plug 'rhysd/vim-clang-format'                              " Vim wrapper plugin for clang-format
Plug 'scrooloose/syntastic'                                " Syntax checking
Plug 'slurps-mad-rips/cmake.vim'                           " Better syntax highlighting for cmake
Plug 'spf13/vim-autoclose'                                 " Matching [({'
Plug 'tommcdo/vim-exchange'                                " cx operator for exchanging text regions
Plug 'tpope/vim-commentary'                                " Comment/uncomment operator
Plug 'tpope/vim-dispatch'                                  " Asynchronous Makes
Plug 'tpope/vim-fugitive'                                  " Git Wrapper
Plug 'tpope/vim-repeat'                                    " Dot operator for plugins
Plug 'tpope/vim-surround'                                  " Surrounding text
Plug 'tpope/vim-unimpaired'                                " Pairs of keyboard mappings for common tasks
Plug 'tpope/vim-vinegar'                                   " netrw improvement
Plug 'vim-scripts/Tabmerge'                                " Merge tabs into splits
Plug 'flazz/vim-colorschemes'                              " Color schemes
Plug 'derekwyatt/vim-scala'                                " Scala syntax
Plug 'luochen1990/rainbow'                                 " Rainbow parenthesis coloring
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }           " Haskell function information
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }           " Displays types and warings/errors
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }             " Haskell completion engine
Plug 'elaforge/fast-tags', { 'for': 'haskell' }            " Ctags generation for Haskell
Plug 'lukerandall/haskellmode-vim', { 'for': 'haskell' }   " Tons of useful things
Plug 'bitc/vim-hdevtools', { 'for': 'haskell' }            " Haskell

if g:platform == "Linux" || g:platform == "Darwin"
    Plug 'SirVer/ultisnips'               " Text snippets
    Plug 'Valloric/YouCompleteMe'
endif


call plug#end()

"" ============================================================================
""                              Plugin Settings
"" ============================================================================

" Clang-format
let g:clang_format#detect_style_file = 1
let g:clang_format#auto_formatexpr = 1
map <C-T> :ClangFormat<CR>
imap <C-T> <C-o>:ClangFormat<CR>
autocmd FileType c,cpp setlocal textwidth=0

" CtrlP
let g:ctrlp_working_path_mode = 'ra'

" Grepper
nmap gs :call Cdroot()<CR><plug>(GrepperOperator)
xmap gs :call Cdroot()<CR><plug>(GrepperOperator)

" Haskellmode-vim
let g:haddock_browser="/usr/bin/firefox"

let g:grepper = {
    \ 'tools':     ['git'],
    \ 'jump':      0,
    \ }

" Netrw
let g:netrw_sort_by = 'name'

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" Set color scheme
set t_Co=256
colorscheme Tomorrow-Night-Eighties

" Syntastic
let g:syntastic_aggregate_errors = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list = 1

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
