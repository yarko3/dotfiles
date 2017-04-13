call plug#begin('~/.vim/plugged')

Plug 'Shougo/vimproc', { 'do': 'make' }                    " Asynchronous command execution library
Plug 'Valloric/ListToggle'                                 " Toggling quickfix and location list
Plug 'Yggdroot/indentLine'                                 " show indent
Plug 'airblade/vim-gitgutter'                              " git changes in gutter
Plug 'bhipple/bde_plugins'                                 " Tools for formatting code according to BDE Standards
Plug 'bhipple/vimux'                                       " Vim and Tmux Integration
Plug 'bling/vim-airline'                                   " Status line
Plug 'christoomey/vim-tmux-navigator'                      " Window/Pane switching with Vim and Tmux
Plug 'conradirwin/vim-bracketed-paste'                     " paste with no fuss
Plug 'ctrlpvim/ctrlp.vim'                                  " File searchin and opening
Plug 'derekwyatt/vim-fswitch', { 'for': 'cpp' }            " Fastswitch (cpp/h toggle)
Plug 'derekwyatt/vim-scala'                                " Scala syntax
Plug 'easymotion/vim-easymotion'                           " I get around round round round
Plug 'godlygeek/tabular'                                   " Align things
Plug 'haya14busa/incsearch.vim'                            " show search as you type
Plug 'junegunn/gv.vim'                                     " commit viewer
Plug 'junegunn/vim-peekaboo'                               " show content of registers
Plug 'luochen1990/rainbow'                                 " Rainbow parenthesis coloring
Plug 'majutsushi/tagbar'                                   " view tags awesomely
Plug 'mbbill/undotree'                                     " history visualizer
Plug 'mhinz/vim-grepper'                                   " Asynchronous Grep -> QuickFix List
Plug 'mhinz/vim-startify'                                  " dope start screen
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': 'cpp' }  " enhanced C++11/14/17 highlighting
Plug 'raimondi/delimitMate'                                " matching brackets
Plug 'rhysd/vim-clang-format'                              " Vim wrapper plugin for clang-format
Plug 'scrooloose/syntastic'                                " Syntax checking
Plug 'terryma/vim-expand-region'                           " quickly expand visual regions
Plug 'tommcdo/vim-exchange'                                " cx operator for exchanging text regions
Plug 'tpope/vim-commentary'                                " Comment/uncomment operator
Plug 'tpope/vim-endwise'                                   " auto-end certain structures
Plug 'tpope/vim-fugitive'                                  " Git Wrapper
Plug 'tpope/vim-repeat'                                    " Dot operator for plugins
Plug 'tpope/vim-rhubarb'                                   " github support
Plug 'tpope/vim-surround'                                  " Surrounding text
Plug 'tpope/vim-unimpaired'                                " Pairs of keyboard mappings for common tasks
Plug 'tpope/vim-vinegar'                                   " netrw improvement
Plug 'vim-airline/vim-airline-themes'                      " Status line themes
Plug 'vim-scripts/Tabmerge'                                " Merge tabs into splits
Plug 'wesQ3/vim-windowswap'                                " swap splits
Plug 'yssl/QFEnter'                                        " quickfix open in different places

if g:platform == "Linux" || g:platform == "Darwin"
    Plug 'Valloric/YouCompleteMe'
endif

call plug#end()

"" ============================================================================
""                              Plugin Settings
"" ============================================================================

" airline
let g:airline_powerline_fonts = 1
let g:airline_theme = 'tomorrow'

" Clang-format
let g:clang_format#detect_style_file = 1
let g:clang_format#auto_formatexpr = 1
map <C-T> :ClangFormat<CR>
autocmd FileType c,cpp setlocal textwidth=0

" CtrlP
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_use_caching = 0
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']

" QFEnter
let g:qfenter_vopen_map = ['<C-v>']
let g:qfenter_hopen_map = ['<C-CR>', '<C-s>', '<C-x>']
let g:qfenter_topen_map = ['<C-t>']

" Grepper
nmap gs :call Cdroot()<CR><plug>(GrepperOperator)
xmap gs :call Cdroot()<CR><plug>(GrepperOperator)

let g:grepper = {
    \ 'tools':     ['git'],
    \ 'jump':      0,
    \ }

" Netrw
let g:netrw_sort_by = 'name'

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" Syntastic
let g:syntastic_aggregate_errors = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_haskell_checkers = ['hlint']
let g:syntastic_java_javac_autoload_maven_classpath = 0

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

" windowswap
let g:windowswap_map_keys = 0 "prevent default bindings

" YouCompleteMe
let g:ycm_server_log_level = 'debug'
let g:ycm_server_keep_logfiles = 0
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_always_populate_location_list = 1
let g:ycm_semantic_triggers = {'haskell' : ['.']}

if g:bbenv != "" && g:platform != "Darwin"
    let g:ycm_seed_identifiers_with_syntax = 1
    let g:ycm_server_python_interpreter = '/opt/bb/bin/python'
else
    let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif

" vim-expand-region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" tagbar
let g:tagbar_autofocus=1
let g:tagbar_sort=0

" indentLine
let g:indentLine_char='|'
" Specify a character to  be used as indent line on the first level
let g:indentLine_first_char='¦'
" Whether the first indent level should be shown
let g:indentLine_showFirstIndentLevel=1
" Specify how much indent level do you want to use for indentLine
let g:indentLine_indentLevel=10
" Whether to show leading spaces
let g:indentLine_leadingSpaceEnabled=0
" Allow to see the concealed in the current cursor line when in normal & indent mode as intended
let g:indentLine_noConcealCursor=''


" easymotion
" turn off default mappings
let g:EasyMotion_do_mapping = 0
" Turn on case insensitive feature
let g:EasyMotion_smartcase = 1
