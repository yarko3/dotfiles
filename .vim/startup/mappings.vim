" Mappings to Load

"==============================================================================
" All Mode Mappings
"

"==============================================================================
" Insert Mode Mappings
"
" Auto-Insertion for closing braces
inoremap {      {}<Left>
inoremap {<CR>  {<CR>}<Esc>O
inoremap {{     {
inoremap {}     {}

" Bael Log
inoremap <F2> BAEL_LOG_DEBUG <<
inoremap <F3> BAEL_LOG_ERROR <<
inoremap <F4> << BAEL_LOG_END;<CR>

" Leaving insert mode with jk
inoremap jk <Esc>

"==============================================================================
" Normal Mode Mappings
"
" <C-l> toggles whether searches are shown
nnoremap <C-l> :set hlsearch! hlsearch?<CR>

" If hl search is off, starting a new search enables it
nnoremap / :set hlsearch<CR>/
nnoremap * :set hlsearch<CR>*
nnoremap ? :set hlsearch<CR>?

" Swap between cpp/h file
nnoremap <silent> <F8> :exec ":e ".(expand("%") =~ ".h$"
            \       ? glob(substitute(expand("%"), ".h$", ".cpp", ""))
            \          : substitute(expand("%"), "\\.cpp$", ".h", ""))<CR>

" Swap to last buffer
nnoremap <silent> <F9> :b#<CR>
" Set <Space> + Character to insert 1 character, then go back to command mode
nnoremap <Space> i_<Esc>r

" Compilation
nnoremap <F3> :Make!<CR>
nnoremap <F4> :!make run<CR>

" Change directory to the directory with the tags file
nnoremap <Leader><Leader>cd :cd ~/mbig/scrape.git<CR>

" Remove trailing whitespace
nnoremap <Leader><Leader>w :call StripTrailingWhitespaces()<CR>
