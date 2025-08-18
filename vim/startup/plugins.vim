call plug#begin('~/.vim/plugged')

" TODO: Look at sodapopcan/vim-twiggy and lambdalisue/gina.vim if I ever need
" to work with git on the daily.

Plug 'FelikZ/ctrlp-py-matcher'                            " fast CtrlP matcher
Plug 'JamshedVesuna/vim-markdown-preview'                 " Markdown rendering
Plug 'Valloric/ListToggle'                                " Toggling quickfix and location list
Plug 'Valloric/MatchTagAlways'                            " always highlight matching tags
Plug 'Yggdroot/indentLine'                                " show indent
Plug 'benmills/vimux'                                     " Vim and Tmux Integration
Plug 'christoomey/vim-tmux-navigator'                     " Window/Pane switching with Vim and Tmux
Plug 'ctrlpvim/ctrlp.vim'                                 " File searchin and opening
Plug 'derekwyatt/vim-fswitch'                             " Fastswitch
Plug 'haya14busa/vim-poweryank'                           " yank over SSH
Plug 'honza/vim-snippets'                                 " snippets repo
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }       " fzf main repo
Plug 'junegunn/fzf.vim'                                   " fuzzy finding of stuff
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
Plug 'raimondi/delimitMate'                               " matching brackets
Plug 'rhysd/conflict-marker.vim'                          " marks merge conflict markers
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
Plug 'wesQ3/vim-windowswap'                               " swap splits
Plug 'whatyouhide/vim-lengthmatters'                      " highlight portion of line that's longer than limit
Plug 'yssl/QFEnter'                                       " quickfix open in different places


if has('nvim')
  Plug 'nvim-lualine/lualine.nvim'
else
  Plug 'bling/vim-airline'
  Plug 'vim-airline/vim-airline-themes'

  let g:airline_powerline_fonts=1
  let g:airline_theme='tomorrow'
endif

if has('nvim')
  Plug 'mhinz/neovim-remote' " for editing commit messages within vim
  Plug 'nvim-lua/plenary.nvim' " dependency for telescope.nvim
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'phaazon/hop.nvim' " I get around round round round

  " LSP
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-nvim-lua'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-vsnip'
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/vim-vsnip'
  Plug 'neovim/nvim-lspconfig'
  Plug 'onsails/lspkind.nvim'

  " Diagnostics
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'folke/trouble.nvim'

  " LLM
  " Dependencies for Avante
  Plug 'MeanderingProgrammer/render-markdown.nvim'
  Plug 'HakonHarnes/img-clip.nvim'
  Plug 'MunifTanjim/nui.nvim'
  Plug 'echasnovski/mini.icons'
  Plug 'echasnovski/mini.pick'
  Plug 'folke/snacks.nvim'
  Plug 'hrsh7th/nvim-cmp'
  Plug 'saadparwaiz1/cmp_luasnip'
  Plug 'ibhagwan/fzf-lua'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'stevearc/dressing.nvim'

  Plug 'yetone/avante.nvim', { 'do': 'make' }
endif

" Install any local plugins.
call SourceIfExists('~/.vim_local/plugins.vim')

call plug#end()

"" ============================================================================
""                              Plugin Settings
"" ============================================================================

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
let g:netrw_liststyle='0'

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

" hop
hi link HopNextKey WarningMsg
hi link HopNextKey1 WarningMsg
hi link HopNextKey2 WarningMsg

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
augroup mycppfiles
  autocmd!
  " Cycle .h -> .cc -> _test.cc -> .h
  autocmd BufEnter *.h let b:fswitchdst  = 'cc,cpp' | let b:fswitchlocs = './'
  autocmd BufEnter *.cc let b:fswitchdst  = 'cc,cpp' | let b:fswitchfnames = '/$/_test/' | let b:fswitchlocs = './'
  autocmd BufEnter *_test.cc let b:fswitchdst  = 'h' | let b:fswitchfnames = '/_test$//' | let b:fswitchlocs = './'
augroup END

augroup mypyfiles
  autocmd!
  " Cycle .py -> _test.py -> .py
  autocmd BufEnter *.py let b:fswitchdst  = 'py' | let b:fswitchfnames = '/$/_test/' | let b:fswitchlocs = './'
  autocmd BufEnter *_test.py let b:fswitchdst  = 'py' | let b:fswitchfnames = '/_test$//' | let b:fswitchlocs = './'
augroup END

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
let g:startify_lists = [
      \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
      \ { 'type': 'files',     'header': ['   MRU']            },
      \ ]

" lengthmatters
call lengthmatters#highlight('ctermbg=239')
let g:lengthmatters_excluded = ['unite', 'tagbar', 'startify', 'gundo', 'vimshell', 'w3m', 'nerdtree', 'help', 'qf', 'dirvish', 'netrw']

" vim-visual-multi
let g:VM_default_mappings = 0
let g:VM_maps = {}
let g:VM_maps['Find Under']         = '<C-n>'
let g:VM_maps['Find Subword Under'] = '<C-n>'

" cpp-enhanced-highlight
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1

" vim-tmux-navigator

" some builds of vim don't respect the vim-tmux-navigator mappings in netrw.
augroup netrw_hack
  autocmd!
  autocmd filetype netrw call NetrwMappings()
augroup END

function! NetrwMappings()
  nnoremap <buffer> <silent> <c-h> :TmuxNavigateLeft<cr>
  nnoremap <buffer> <silent> <c-j> :TmuxNavigateDown<cr>
  nnoremap <buffer> <silent> <c-k> :TmuxNavigateUp<cr>
  nnoremap <buffer> <silent> <c-l> :TmuxNavigateRight<cr>
endfunction

" vim-markdown-preview
let vim_markdown_preview_github=1
let vim_markdown_preview_browser='Google Chrome'
let vim_markdown_preview_hotkey='<C-m>'

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

if has("nvim")
lua << EOF

-- telescope
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        preview_width = 80
        }
    },
    mappings = {
      i = {
        ["<c-j>"] = actions.move_selection_next,
        ["<c-k>"] = actions.move_selection_previous,
        ["<c-q>"] = actions.send_to_qflist,
      },
      n = {
        ["<c-j>"] = actions.move_selection_next,
        ["<c-k>"] = actions.move_selection_previous,
        ["<c-q>"] = actions.send_to_qflist,
      },
    },
  }
}

require'hop'.setup{
  -- enable hopping across splits/windows
  multi_windows = true
}

-- LSP
vim.opt.completeopt = { "menu", "menuone", "noselect" }
require("lsp")

-- Diagnostics
require("diagnostics")

-- Avante Dependencies
require('render-markdown').setup{
 file_types = { "markdown", "Avante" },
}
require('mini.icons').setup{}
require('mini.pick').setup{}

require('lualine').setup{
  options = {theme = 'tomorrow_night'},
}

EOF
endif
