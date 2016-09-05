call plug#begin('~/.vim/plugged')

Plug 'Shougo/vimproc', { 'do': 'make' }                    " Asynchronous command execution library
Plug 'Valloric/ListToggle'                                 " Toggling quickfix and location list
Plug 'bhipple/bde_plugins'                                 " Tools for formatting code according to BDE Standards
Plug 'bhipple/vim-snippets'                                " My snippets fork
Plug 'bhipple/vimux'                                       " Vim and Tmux Integration
Plug 'bling/vim-airline'                                   " Status line
Plug 'chazmcgarvey/vimcoder'                               " Topcoder Vim Plugin
Plug 'christoomey/vim-tmux-navigator'                      " Window/Pane switching with Vim and Tmux
Plug 'ctrlpvim/ctrlp.vim'                                  " File searchin and opening
Plug 'derekwyatt/vim-fswitch', { 'for': 'cpp' }            " Fastswitch (cpp/h toggle)
Plug 'ivanov/vim-ipython', { 'for': 'python' }             " Vim + IPython Notebook integration
Plug 'jceb/vim-orgmode'                                    " Emacs orgmode port
Plug 'justinmk/vim-syntax-extra'                           " Flex and Bison syntax highlighting
Plug 'majutsushi/tagbar'                                   " Using for JavaScript
Plug 'mhinz/vim-grepper'                                   " Asynchronous Grep -> QuickFix List
Plug 'rhysd/vim-clang-format'                              " Vim wrapper plugin for clang-format
Plug 'scrooloose/syntastic'                                " Syntax checking
Plug 'slurps-mad-rips/cmake.vim'                           " Better syntax highlighting for cmake
Plug 'spf13/vim-autoclose'                                 " Matching [({'
Plug 'tommcdo/vim-exchange'                                " cx operator for exchanging text regions
Plug 'tpope/vim-abolish'                                   " Coercion and Subvert
Plug 'tpope/vim-commentary'                                " Comment/uncomment operator
Plug 'tpope/vim-dispatch'                                  " Asynchronous Makes
Plug 'tpope/vim-fugitive'                                  " Git Wrapper
Plug 'tpope/vim-repeat'                                    " Dot operator for plugins
Plug 'tpope/vim-surround'                                  " Surrounding text
Plug 'tpope/vim-unimpaired'                                " Pairs of keyboard mappings for common tasks
Plug 'tpope/vim-vinegar'                                   " netrw improvement
Plug 'vim-scripts/Tabmerge'                                " Merge tabs into splits
Plug 'flazz/vim-colorschemes'                              " Color schemes

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

" HIndent
let g:hindent_style = "cramer"

let g:grepper = {
    \ 'tools':     ['git'],
    \ 'jump':      1,
    \ }

" Netrw
let g:netrw_sort_by = 'name'

" ListToggle
let g:lt_location_list_toggle_map = '<leader>l'
let g:lt_quickfix_list_toggle_map = '<leader>q'

" Set color scheme
set t_Co=256
colorscheme Tomorrow-Night-Eighties

" SLIMV
let g:slimv_repl_split=4 " Split Vertically

" Syntastic
let g:syntastic_aggregate_errors = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_always_populate_loc_list = 1

" Tagbar
let g:tagbar_left = 1

" UltiSnips
" Magic to make the <enter> key expand snippes, even with YouCompleteMe installed.
let g:UltiSnipsExpandTrigger = "<nop>"
let g:ulti_expand_or_jump_res = 0
function! ExpandSnippetOrCarriageReturn()
    let snippet = UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res > 0
        return snippet
    else
        return "\<CR>"
    endif
endfunction
inoremap <expr> <CR> pumvisible() ? "<C-R>=ExpandSnippetOrCarriageReturn()<CR>" : "\<CR>"

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
