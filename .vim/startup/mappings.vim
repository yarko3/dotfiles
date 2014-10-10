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


"" ============================================================================
""                        Normal/Visual Mode Mappings
"" ============================================================================
" clang-format
map <C-T> :pyf ~/bin/clang-format.py<CR>
imap <C-T> <ESC>:pyf ~/bin/clang-format.py<CR>

" YouCompleteMe
nnoremap <F5> :YcmForceCompileAndDiagnostics<CR>

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

" Compilation and testing with Dispatch
if(g:bbenv == "")
    nnoremap <F4> :Make all<CR>:Make run<CR>:Copen<CR>/FAILED<CR>
else
    nnoremap <F3> :Make gtest<CR>
    nnoremap <F4> :Make stage<CR>
endif


"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
nmap <Leader>* yiw/^<C-r><C-0>\s*:<CR>
nmap <Leader>/ /^\s*:\c<Left><Left><Left><Left><Left><Left>
nnoremap <Leader>cdf :call Cdfile()<CR>
nnoremap <Leader>cdg :call Cdgit()<CR>
nnoremap <Leader>cds :call Cdscraper()<CR>
nnoremap <Leader>cmt :call CmtSection("")<Left><Left>
nnoremap <Leader>e :e %:h<CR>
nnoremap <Leader>ff :FSHere<CR>
nnoremap <Leader>fh :FSSplitLeft<CR>
nnoremap <Leader>fj :FSSplitBelow<CR>
nnoremap <Leader>fk :FSSplitAbove<CR>
nnoremap <Leader>fl :FSSplitRight<CR>
nnoremap <Leader>fmt :call Bde_Format()<CR>
nnoremap <Leader>gg :Ggrep <C-r><C-w><CR>:copen<CR>
nnoremap <Leader>h :set hlsearch! hlsearch?<CR>
nnoremap <Leader>ii O// Application Includes<CR>// BDE Includes<CR>// System Includes<ESC>
nnoremap <Leader>ia O// Application Includes<ESC>
nnoremap <Leader>ib O// BDE Includes<ESC>
nnoremap <Leader>is O// System Includes<ESC>
nnoremap <Leader>jc :YcmCompleter GoToDeclaration<CR>
nnoremap <Leader>jf :YcmCompleter GoToDefinition<CR>
nnoremap <Leader>ji :YcmCompleter GoToImprecise<CR>
nnoremap <Leader>jj :YcmCompleter GoTo<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>
nnoremap <Leader>tb :TagbarToggle<CR>
nnoremap <Leader>te :tabe %:h<CR>
nnoremap <Leader>tm :Tabmerge right<CR>
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>

source ~/.vim/bundle/bde_plugins/bde_format.vim
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>

" Note - l and q are used for the location list and quickfix toggle by ListToggle
