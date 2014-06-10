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

" Bael Log Shortcuts
inoremap <F2> BAEL_LOG_TRACE << 
inoremap <F3> BAEL_LOG_DEBUG << 
inoremap <F4> BAEL_LOG_ERROR << 
inoremap <F5> << BAEL_LOG_END;<ESC>

" Leaving insert mode with jj
inoremap jj <Esc><Right>

"==============================================================================
" Normal Mode Mappings
"
" <C-l> toggles whether searches are shown
nnoremap <C-l> :set hlsearch! hlsearch?<CR>

" If hl search is off, starting a new search or moving enables it
nnoremap / :set hlsearch<CR>/
nnoremap * :set hlsearch<CR>*
nnoremap # :set hlsearch<CR>#
nnoremap ? :set hlsearch<CR>?
nnoremap n :set hlsearch<CR>n
nnoremap N :set hlsearch<CR>N

" Swap to last buffer
nnoremap <silent> <F8> :b#<CR>

" Set <Space> + Character to insert 1 character, then go back to command mode
nnoremap <Space> i_<Esc>r

" Compilation
nnoremap <F3> :Make!<CR>
nnoremap <F4> :make<CR>
nnoremap <F5> :!./*.tsk<CR>

"==============================================================================
" Leader Mappings (Sorted)
"
nmap <Leader>* yiw/^<C-r><C-0>\s*:<CR>
nmap <Leader>/ /^\s*:\c<Left><Left><Left><Left><Left><Left>
nnoremap <Leader>cdf :call Cdfile()<CR>
nnoremap <Leader>cdg :call Cdgit()<CR>
nnoremap <Leader>cds :call Cdscraper()<CR>
nnoremap <Leader>e :e %:h<CR>9j
nnoremap <Leader>ff :FSHere<CR>
nnoremap <Leader>fh :FSSplitLeft<CR>
nnoremap <Leader>fj :FSSplitBelow<CR>
nnoremap <Leader>fk :FSSplitAbove<CR>
nnoremap <Leader>fl :FSSplitRight<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>9j
nnoremap <Leader>t :TagbarToggle<CR>
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>9j
nnoremap <Leader>w :call StripTrailingWhitespaces()<CR>
