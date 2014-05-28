" vim settings

set nocompatible
let g:platform = GetPlatform()

" Editing
syntax on
set autoindent
set cindent
set backspace=indent,eol,start

" ctags locations
set tags=./tags;~/mbig/scrape.git/msgscrape/scraper/tags

" runtime path search for Ex
set ru

" Fixing tabs
set tabstop=4
set expandtab
set shiftwidth=4

" Allow switching off modified buffers without warning
set hidden

" Incremental Search and Highlighting Results
set incsearch
set hlsearch

" Show line numbers
set number

" Show tab whitespace characters
set listchars=tab:>-,trail:-
set list!

" Using the mouse
set mouse=a

" Configure :make with plink
"set makeprg=rm\ -f\ *.tsk\ &&\ ~/shellscripts/bld
set makeprg=plink\ *.mk

" Make vsplit split the new window to the right, not left
set splitright

" Autosave before :make and other commands; autoreload when file mod
set autowrite
set autoread

" Set the folding method
set foldmethod=manual
set foldnestmax=3
set foldminlines=10

" Removing GUI Options on gvim
set guioptions-=l    " Left scrollbar (without split)
set guioptions-=L    " Left scrollbar
set guioptions-=r    " Right scrollbar (without split)
set guioptions-=R    " Right scrollbar
set guioptions-=T    " Toolbar

" Set path for file searches
set path+=/home/bhipple/mbig/scrape.git/**

" Automatically open the QuickFix Window after a make
autocmd QuickFixCmdPost *make* cwindow

" in Makefiles, don't expand tabs to spaces and reset the standard tab
" length because Makefiles require the indentation of tabs for targets
autocmd FileType make setlocal noexpandtab shiftwidth=8

" Set automatic indentation format for XML files
autocmd FileType xml exe ":silent %!xmllint --format --recover - 2>/dev/null"

" Check for lines that exceed 80 chars . . . too laggy
"autocmd BufRead,BufNewFile   *.c,*.h,*.cpp au BufWinEnter * let w:m1=matchadd('Search', '\%<81v.\%>77v', -1)
"autocmd BufRead,BufNewFile   *.c,*.h,*.cpp au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)
