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

" Leaving insert mode with jj
inoremap jj <Esc>

"==============================================================================
" Normal Mode Mappings
"
" <C-l> toggles whether searches are shown
nnoremap <C-l> :set hlsearch! hlsearch?<CR>

" If hl search is off, starting a new search or moving enables it
nnoremap / :set hlsearch<CR>/
nnoremap * :set hlsearch<CR>*
nnoremap ? :set hlsearch<CR>?
nnoremap n :set hlsearch<CR>n
nnoremap N :set hlsearch<CR>N

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
nnoremap <F4> :make<CR>
nnoremap <F5> :!./*.tsk<CR>

"==============================================================================
" Leader Mappings
"
" cd to current file's directory
nnoremap <Leader>cdf :cd %:h<CR>:pwd<CR>

" Change directory to the directory with the tags file
nnoremap <Leader>cdt :cd ~/mbig/scrape.git<CR>

" Find a patterns.cfg definition
nnoremap <Leader>fd :set hlsearch<CR>yiw/^<C-r><C-0>\s*:<CR>

" Open a netrw window on the current file's directory
nnoremap <Leader>e. :e %:h<CR>9j
nnoremap <Leader>ve. :vsp<CR>:e %:h<CR>9j
nnoremap <Leader>se. :sp<CR>:e %:h<CR>9j

nnoremap <Leader>t :TagbarToggle<CR>

" Remove trailing whitespace
nnoremap <Leader>w :call StripTrailingWhitespaces()<CR>
