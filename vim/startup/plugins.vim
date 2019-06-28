call plug#begin('~/.vim/plugged')

" TODO: Look at sodapopcan/vim-twiggy and lambdalisue/gina.vim if I ever need
" to work with git on the daily.

Plug 'FelikZ/ctrlp-py-matcher'                            " fast CtrlP matcher
Plug 'Valloric/ListToggle'                                " Toggling quickfix and location list
Plug 'Valloric/MatchTagAlways'                            " always highlight matching tags
Plug 'Yggdroot/indentLine'                                " show indent
Plug 'benmills/vimux'                                     " Vim and Tmux Integration
Plug 'bling/vim-airline'                                  " Status line
Plug 'christoomey/vim-tmux-navigator'                     " Window/Pane switching with Vim and Tmux
Plug 'ctrlpvim/ctrlp.vim'                                 " File searchin and opening
Plug 'derekwyatt/vim-fswitch', { 'for': 'cpp' }           " Fastswitch (cpp/h toggle)
Plug 'easymotion/vim-easymotion'                          " I get around round round round
Plug 'haya14busa/vim-poweryank'                           " yank over SSH
Plug 'henrik/vim-indexed-search'                          " show count for search results
Plug 'honza/vim-snippets'                                 " snippets repo
Plug 'junegunn/vim-easy-align'                            " align things
Plug 'junegunn/vim-peekaboo'                              " show content of registers
Plug 'kana/vim-textobj-user'                              " user-defined text objects (dependency for other plugins)
Plug 'luochen1990/rainbow'                                " Rainbow parenthesis coloring
Plug 'machakann/vim-highlightedyank'                      " highlight on yank
Plug 'majutsushi/tagbar'                                  " view ctags awesomely
Plug 'mbbill/undotree'                                    " history visualizer
Plug 'mg979/vim-visual-multi'                             " multiple cursors
Plug 'mhinz/vim-grepper'                                  " Asynchronous Grep -> QuickFix List
Plug 'mhinz/vim-signify'                                  " changes in gutter
Plug 'mhinz/vim-startify'                                 " dope start screen
Plug 'michaeljsmith/vim-indent-object'                    " indentation-level text objects (ai/I, ii/I)
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': 'cpp' } " enhanced C++11/14/17 highlighting
Plug 'osyo-manga/vim-brightest'                           " highlight/underline current word
Plug 'prabirshrestha/async.vim'                           " async library for lsp
Plug 'prabirshrestha/vim-lsp'                             " language server protocol support
Plug 'raimondi/delimitMate'                               " matching brackets
Plug 'roxma/vim-tmux-clipboard'                           " integrate vim's yank register with tmux
Plug 'sirver/ultisnips'                                   " snippet engine with integration into ycm; needs vim compiled with python
Plug 'szw/vim-maximizer'                                  " maximize a split
Plug 'talek/obvious-resize'                               " resize splits
Plug 'terryma/vim-expand-region'                          " quickly expand visual regions
Plug 'tmux-plugins/vim-tmux-focus-events'                 " focusing help with tmux and certain plugins
Plug 'tommcdo/vim-exchange'                               " cx operator for exchanging text regions
Plug 'tpope/vim-commentary'                               " Comment/uncomment operator
Plug 'tpope/vim-fugitive'                                 " Git Wrapper
Plug 'tpope/vim-repeat'                                   " Dot operator for plugins
Plug 'tpope/vim-surround'                                 " Surrounding text
Plug 'tpope/vim-vinegar'                                  " netrw improvement
Plug 'vim-airline/vim-airline-themes'                     " Status line themes
Plug 'w0rp/ale'                                           " Syntax checking
Plug 'wesQ3/vim-windowswap'                               " swap splits
Plug 'whatyouhide/vim-lengthmatters'                      " highlight portion of line that's longer than limit
Plug 'yssl/QFEnter'                                       " quickfix open in different places

if g:platform == "Linux" && !AtWork()
  Plug 'Valloric/YouCompleteMe'
endif

call plug#end()

"" ============================================================================
""                              Plugin Settings
"" ============================================================================

" airline
let g:airline_powerline_fonts=1
let g:airline_theme='tomorrow'

" CtrlP
let g:ctrlp_use_caching = 0

if executable('rg')
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
elseif executable('ag')
  let g:ctrlp_user_command = 'ag --literal --files-with-matches --nocolor --hidden -g "" %s'
else
let g:ctrlp_user_command = {
    \ 'types': {
        \ 1: ['.git/', 'cd %s && git ls-files -oc --exclude-standard'],
        \ 2: ['.hg/', 'hg --cwd %s locate -I .'],
    \ },
    \ 'fallback': 'find %s -type f'
\ }
endif

" cd into local working directory root if present,
" use CtrlP to find project root (.git, .hg, etc.) otherwise.
function! MyCtrlP()
let l:local_pwd = GetLocalRoot()
if l:local_pwd == ""
  let g:ctrlp_working_path_mode = 'ra'
else
  exec "cd " . l:local_pwd
  let g:ctrlp_working_path_mode = ''
endif
:CtrlP
endfunction

let g:ctrlp_cmd = 'call MyCtrlP()'

" increase maximum window height
let g:ctrlp_match_window = 'max:20'

" Quick matching function.
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

" QFEnter
let g:qfenter_keymap={}
let g:qfenter_keymap.vopen=['<C-v>']
let g:qfenter_keymap.hopen=['<C-x>']
let g:qfenter_keymap.topen=['<C-t>']

" Grepper
nmap gs :call CdRoot()<CR><plug>(GrepperOperator)
xmap gs :call CdRoot()<CR><plug>(GrepperOperator)

let my_grepper_options = {
      \ 'tools': ['rg', 'ag'],
      \ 'rg': {
      \   'grepprg': 'rg --vimgrep --no-heading',
      \ },
      \ 'ag': {
      \   'grepprg': 'ag --nogroup --nocolor --column',
      \ },
      \ 'jump': 0,
      \ }

let g:grepper = my_grepper_options
let g:grepper.operator = my_grepper_options

" Netrw
let g:netrw_liststyle='1'
let g:netrw_timefmt='%d %b %Y %T %Z'
let g:netrw_sizestyle='h'

" ListToggle
let g:lt_location_list_toggle_map='<leader>l'
let g:lt_quickfix_list_toggle_map='<leader>q'

" Rainbow coloring
let g:rainbow_active=1
let g:rainbow_conf={
      \  'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
      \  'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
      \  'operators': '_,_',
      \  'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
      \  'separately': {
      \     '*': {},
      \     'tex': {
      \         'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/'],
      \     },
      \     'lisp': {
      \         'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
      \     },
      \     'vim': {
      \         'parentheses': ['start=/(/ end=/)/', 'start=/\[/ end=/\]/', 'start=/{/ end=/}/ fold', 'start=/(/ end=/)/ containedin=vimFuncBody', 'start=/\[/ end=/\]/ containedin=vimFuncBody', 'start=/{/ end=/}/ fold containedin=vimFuncBody'],
      \     },
      \     'html': {
      \         'parentheses': ['start=/\v\<((area|base|br|col|embed|hr|img|input|keygen|link|menuitem|meta|param|source|track|wbr)[ >])@!\z([-_:a-zA-Z0-9]+)(\s+[-_:a-zA-Z0-9]+(\=("[^"]*"|'."'".'[^'."'".']*'."'".'|[^ '."'".'"><=`]*))?)*\>/ end=#</\z1># fold'],
      \     },
      \     'css': 0,
      \  }
      \}

" windowswap
" prevent default bindings
let g:windowswap_map_keys=0

" YouCompleteMe
let g:ycm_server_log_level='debug'
let g:ycm_server_keep_logfiles=0
let g:ycm_confirm_extra_conf=0
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_always_populate_location_list=1
let g:ycm_goto_buffer_command = 'vertical-split'
let g:ycm_complete_in_comments = 0

" For some unicode reason ycm errors on peekaboo.
let g:ycm_filetype_blacklist = { 'peekaboo': 1, 'startify': 1 }

" Configure quickfix window settings when opened from YCM.
function! s:CustomizeYcmQuickFixWindow()
  " Set the window height.
  15wincmd _
endfunction
autocmd User YcmQuickFixOpened call s:CustomizeYcmQuickFixWindow()

if !AtWork()
  let g:ycm_global_ycm_extra_conf='~/.vim/bundle/YouCompleteMe/cpp/.ycm_extra_conf.py'
endif

" vim-expand-region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)
let g:expand_region_text_objects = {
      \ 'iw'  :0,
      \ 'iW'  :0,
      \ 'i"'  :1,
      \ 'i''' :1,
      \ 'i]'  :1,
      \ 'ib'  :1,
      \ 'iB'  :1,
      \ 'il'  :0,
      \ 'ip'  :0,
      \ 'ie'  :0,
      \ }
call expand_region#custom_text_objects({
      \ 'a]' :1,
      \ 'ab' :1,
      \ 'aB' :1,
      \ 'ii' :1,
      \ 'ai' :1,
      \ })

" tagbar
let g:tagbar_autofocus=1
let g:tagbar_autoclose=1
let g:tagbar_sort=0
let g:tagbar_width=50

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
" ignore startify
let g:indentLine_fileTypeExclude=['startify']

" easymotion
" turn off default mappings
let g:EasyMotion_do_mapping=0
" turn on case insensitive feature
let g:EasyMotion_smartcase=1
" set colors
hi link EasyMotionTarget WarningMsg
hi link EasyMotionShade  Comment

" snippets
let g:UltiSnipsExpandTrigger="<nop>"
let g:ulti_expand_or_jump_res=0
function! ExpandSnippetOrCarriageReturn()
  let snippet=UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res > 0
    return snippet
  else
    return "\<CR>"
  endif
endfunction
inoremap <expr> <CR> pumvisible() ? "\<C-R>=ExpandSnippetOrCarriageReturn()\<CR>" : "\<CR>"
let g:UltiSnipsJumpForwardTrigger='<Tab>'
let g:UltiSnipsJumpBackwardTrigger='<S-Tab>'

" Ale
" turn off convention pylint messages and misc
let g:ale_python_pylint_options='--disable=C --disable=W0311'
let g:ale_java_checkstyle_options='-c ~/.vim/config/checkstyle_custom_checks.xml'
let g:ale_linters={
      \  'cpp': ['cppcheck'],
      \  'java': ['checkstyle'],
      \  'zsh': ['shellcheck'],
      \  'markdown': [],
      \}
highlight ALEErrorSign   ctermfg=9  ctermbg=235
highlight ALEWarningSign ctermfg=11 ctermbg=235

" vim-highlightedyank
if !has('nvim')
  map y <Plug>(highlightedyank)
endif

" obvious resize
let g:obvious_resize_default=10

" undotree
" grab focus on toggle
let g:undotree_SetFocusWhenToggle = 1

" maximizer
let g:maximizer_set_default_mapping = 0

" vimux
let g:VimuxOrientation = "h"
let g:VimuxHeight = "40"

" fswitch
au BufEnter *.h let b:fswitchdst  = 'cc,cpp'
au BufEnter *_test.cc let b:fswitchfnames = '/_test$//'
au BufEnter *.cc let b:fswitchdst  = 'h'

" signify
highlight SignifySignAdd    cterm=bold ctermbg=235  ctermfg=119
highlight SignifySignDelete cterm=bold ctermbg=235  ctermfg=167
highlight SignifySignChange cterm=bold ctermbg=235  ctermfg=227
" refresh on focus
let g:signify_update_on_focusgained = 1

let g:signify_vcs_cmds = {
\  'hg':       'hg diff --color=never --config defaults.diff= --nodates -U0 -- %f',
\}

" brightest
let g:brightest#pattern = '\k\+'
let g:brightest#highlight = {
\   "group" : "BrightestUnderline",
\}
let g:brightest#enable_highlight_all_window = 1

" Fugitive
" automatically delete fugitive buffers when leaving them
autocmd BufReadPost fugitive://* set bufhidden=delete

" startify
" speed up a bit
let g:startify_enable_unsafe = 1

" lengthmatters
call lengthmatters#highlight('ctermbg=239')
let g:lengthmatters_excluded = ['unite', 'tagbar', 'startify', 'gundo', 'vimshell', 'w3m', 'nerdtree', 'help', 'qf', 'dirvish', 'netrw']

" vim-visual-multi
let g:VM_default_mappings = 0
let g:VM_maps = {}
let g:VM_maps['Find Under']         = '<C-n>'
let g:VM_maps['Find Subword Under'] = '<C-n>'

" vim-indexed-search
" disable default mappings
let g:indexed_search_mappings = 0
let g:indexed_search_numbered_only = 1

" Stole a bunch of their code to only enable mappings for /? and not the rest
noremap  <Plug>(indexed-search-index)  <Nop>
nnoremap <Plug>(indexed-search-index)  :ShowSearchIndex<CR>
xnoremap <Plug>(indexed-search-index)  :<C-u>ShowSearchIndex<CR>gv

function! s:vim_indexed_search_should_unfold()
  return has('folding') && &fdo =~ 'search\|all'
endfunction

function! s:vim_indexed_search_after()
  return (s:vim_indexed_search_should_unfold() ? 'zv' : '')
        \ .('zz')
        \ ."\<Plug>(indexed-search-index)"
endfunction

map  <expr> <Plug>(indexed-search-after)  <SID>vim_indexed_search_after()
imap        <Plug>(indexed-search-after)  <Nop>

cmap <expr> <CR> "\<CR>" . (getcmdtype() =~ '[/?]' ? "\<Plug>(indexed-search-after)" : '')
