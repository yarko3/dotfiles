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
set ttyfast

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
    set makeprg=make
else
    set makeprg=toolkit-remote\ nylxdev2\ gmake\ -j
endif

" Set path for file searches
set path+=/home/bhipple/mbig/**

" Ignore whitespace on diffs
set diffopt+=iwhite

" Smart case sensitivity
set ignorecase
set smartcase

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

" Ignoring files (see :help wildignore)
set wildignore+=*.o,*.d,nohup.out,tags

"" ============================================================================
""                               Auto Commands
"" ============================================================================
" Automatically open the QuickFix Window after a make
autocmd QuickFixCmdPost *make* cwindow

" in Makefiles, don't expand tabs to spaces and reset the standard tab
" length because Makefiles require the indentation of tabs for targets
autocmd FileType make setlocal noexpandtab shiftwidth=8

" Set automatic indentation format for XML files
autocmd FileType xml setlocal equalprg=xmllint\ --format\ -
