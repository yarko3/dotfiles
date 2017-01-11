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
" copy the previous indentation on autoindenting
set copyindent
set cindent
set backspace=indent,eol,start

" oh no, mouse
set mouse=a

" Backup directory for swp files
set noswapfile
set directory=""

" runtime path search for Ex
set ru

" Fixing tabs
set tabstop=4
set expandtab
set shiftwidth=4
" makes indenting a multiple of shiftwidth
set shiftround

" Allow switching off modified buffers without warning
set hidden

" Autosave before :make and other commands; autoreload when file mod
set autowrite
set autoread

" Configure the :make command
set makeprg=make

" Set path for file searches
set path+=/home/ysenyuta/mbig/**

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
set wildignore+=*.o,*.d,00*,nohup.out,tags,.hs-tags,*.hi,*.gcno,*.gcda,*.fasl,*.pyc

" Number of lines to scroll past when the cursor scrolls off the screen
set scrolloff=2

" Extend functionality of the % key's matching
runtime macros/matchit.vim

" Tool to use for Grepper
set grepprg="git"

" What to use for gq
set formatprg=par\ -w80

" Additional words for the spell checker
set spellfile=~/.vim/spell/extra-words.add

" stores undo state even when files are closed (in undodir)
if !isdirectory($HOME . '/.vim/backups')
    call mkdir($HOME . '/.vim/backups', 'p')
endif
set undodir=~/.vim/backups
set undofile

"" ============================================================================
""                                Appearance
"" ============================================================================
" Show line numbers
set number

" show the cursor position
set ruler

" no linewrap
set nowrap

" Show tab and trailing whitespace characters
set listchars=tab:>-,trail:-
set list!

" Make splitting more natural
set splitright
set splitbelow

" Incremental Search and Highlighting Results
set incsearch
set hlsearch

" Always show status bar
set laststatus=2

" Set the folding method
set foldmethod=manual
set foldnestmax=3
set foldminlines=10

" double click to highlight all occurrences
nnoremap <silent> <2-LeftMouse> :let @/='\V\<'.escape(expand('<cword>'), '\').'\>'<cr>:set hls<cr>

" no beeps
set visualbell

" highlights the current line
set cursorline

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

" set commentstring to not be /* for cpp
autocmd FileType cpp setlocal commentstring=//\ %s
