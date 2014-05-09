" vim settings

" Bloomberg Settings
syntax on
set et
set ai
set cin
set ru
set bs=indent,eol,start
set sw=4

set nocompatible

" Fixing tabs
set tabstop=4
set expandtab
set shiftwidth=4

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

" Make vsplit split the new window to the right, not left
set splitright

" Autosave before :make and other commands; autoreload when file mod
set autowrite
set autoread

" Set the folding method
set foldmethod=manual

" Stop comments automatically going on newlines
setlocal comments-=://
setlocal comments+=f://

" Removing GUI Options on gvim
set guioptions-=l    " Left scrollbar (without split)
set guioptions-=L    " Left scrollbar
set guioptions-=r    " Right scrollbar (without split)
set guioptions-=R    " Right scrollbar
set guioptions-=T    " Toolbar

" Set path for file searches
"set path+=./**
"set path+=../**
set path+=/home/bhipple/mbig/scrape.git/**

" Check for lines that exceed 80 chars . . . too laggy
"autocmd BufRead,BufNewFile   *.c,*.h,*.cpp au BufWinEnter * let w:m1=matchadd('Search', '\%<81v.\%>77v', -1)
"autocmd BufRead,BufNewFile   *.c,*.h,*.cpp au BufWinEnter * let w:m2=matchadd('ErrorMsg', '\%>80v.\+', -1)

" CTAGS for Vim @ BB
" source /bbsrc/princeton/skunk/vim/cursor.vim  <-- Better to modify the makefile

