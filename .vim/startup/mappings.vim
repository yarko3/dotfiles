"" ============================================================================
""                           Insert Mode Mappings
"" ============================================================================
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

" clang-format
imap <C-k> <ESC>:pyf ~/bin/clang-format.py<CR>i

"" ============================================================================
""                        Normal/Visual Mode Mappings
"" ============================================================================
" clang-format
map <C-k> :pyf ~/bin/clang-format.py<CR>

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

"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
nmap <Leader>* yiw/^<C-r><C-0>\s*:<CR>
nmap <Leader>/ /^\s*:\c<Left><Left><Left><Left><Left><Left>
nnoremap <Leader>cdf :call Cdfile()<CR>
nnoremap <Leader>cdg :call Cdgit()<CR>
nnoremap <Leader>cds :call Cdscraper()<CR>
nnoremap <Leader>cmt :call CmtSection("")<Left><Left>
nnoremap <Leader>e :e %:h<CR>9j
nnoremap <Leader>ff :FSHere<CR>
nnoremap <Leader>fh :FSSplitLeft<CR>
nnoremap <Leader>fj :FSSplitBelow<CR>
nnoremap <Leader>fk :FSSplitAbove<CR>
nnoremap <Leader>fl :FSSplitRight<CR>
nnoremap <Leader>fmt :call Bde_Format()<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>9j
nnoremap <Leader>tb :TagbarToggle<CR>
nnoremap <Leader>te :tabe %:h<CR>9j
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>9j

source ~/.vim/bundle/bde_plugins/bde_format.vim
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>

" Note - l and q are used for the location list and quickfix toggle by ListToggle
