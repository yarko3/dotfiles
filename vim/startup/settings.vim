"" ============================================================================
""                                 Settings
"" ============================================================================
set nocompatible

"" ============================================================================
""                                  Globals
"" ============================================================================
" Determine Environment
let g:platform = GetPlatform()
let g:bbenv = GetBBENV()

" To enable the saving and restoring of screen positions.
let g:screen_size_restore_pos = 1

"" ============================================================================
""                            Editing and Moving
"" ============================================================================
syntax on
set autoindent
set cindent
set backspace=indent,eol,start

" Backup directory for swp files
set noswapfile
set directory=""

" Faster terminal scrolling?  TBD
"set ttyfast

" runtime path search for Ex
set ru

" Fixing tabs
set tabstop=4
set expandtab
set shiftwidth=4

" Allow switching off modified buffers without warning
set hidden

" Autosave before :make and other commands; autoreload when file mod
set autowrite
set autoread

" Configure the :make command
if(g:bbenv == "")
    set makeprg=gmake
else
    set makeprg=brh-make
endif

" Set path for file searches
set path+=/home/bhipple/mbig/**

" Ignore whitespace on diffs
set diffopt+=iwhite

" Smart case sensitivity
set ignorecase
set smartcase

" Fix background color
set t_ut=

" When multiple completions are possible, show all
set wildmenu

" Complete only up to point of ambiguity, like the shell does
set wildmode=list:longest

" Ignoring files (see :help wildignore)
set wildignore+=*.o,*.d,00*,nohup.out,tags,*.hi,*.gcno,*.gcda,*.fasl

" Number of lines to scroll past when the cursor scrolls off the screen
set scrolloff=2

" Extend functionality of the % key's matching
runtime macros/matchit.vim

" Tool to use for Grepper
set grepprg="git"

" What to use for gq
set formatprg=par\ -w80

" Use the system clipboard for yank and put!
set clipboard=unnamedplus

"" ============================================================================
""                                Appearances
"" ============================================================================
" Show line numbers
set number

" Show tab and trailing whitespace characters
set listchars=tab:>-,trail:-
set list!

" Make vsplit split the new window to the right, not left
set splitright

" Incremental Search and Highlighting Results
set incsearch
set hlsearch

" Set the folding method
set foldmethod=manual
set foldnestmax=3
set foldminlines=10

"" ============================================================================
""                               Auto Commands
"" ============================================================================
" Automatically open the QuickFix Window after a make
autocmd QuickFixCmdPost *make* cwindow

" Make
autocmd FileType make setlocal noexpandtab shiftwidth=8

" XML
autocmd FileType xml setlocal equalprg=xmllint\ --format\ -

" Markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Haskell
au BufEnter *.hs compiler ghc
