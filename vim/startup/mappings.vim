"" ============================================================================
""                             All Mode Mappings
"" ============================================================================
" Bael log category at the start of function
map <F6> <ESC>[[oBAEL_LOG_SET_CATEGORY(LOG_CATEGORY);<ESC><C-o>

" clang-format
map <C-T> :pyf ~/bin/clang-format.py<CR>
imap <C-T> <C-o>:pyf ~/bin/clang-format.py<CR>

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

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
" YouCompleteMe
nnoremap <F9> :YcmForceCompileAndDiagnostics<CR>

" If hl search is off, starting a new search or moving enables it
nnoremap * :set hlsearch<CR>*
nnoremap # :set hlsearch<CR>#
nnoremap n :set hlsearch<CR>n
nnoremap N :set hlsearch<CR>N
nnoremap / :set hlsearch<CR>/
nnoremap ? :set hlsearch<CR>?

" Also, commmand and search history by default
"nnoremap : q:i
"nnoremap / :set hlsearch<CR>q/i
"nnoremap ? :set hlsearch<CR>q?i

" Swap to last buffer
nnoremap <silent> <F8> :b#<CR>

" Compilation and testing with Dispatch
nnoremap <F2> :call MakefileSearch()<CR>:Make clean<CR>
if(g:bbenv == "")
    nnoremap <F4> :call MakefileSearch()<CR>:Make all<CR>:Make run<CR>:Copen<CR>/FAILED<CR>
else
    nnoremap <F3> :call MakefileSearch()<CR>:Make test<CR>
    nnoremap <F4> :call MakefileSearch()<CR>:Make<CR>
endif

noremap <F5> :cex[]<CR>:cclose<CR>


"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
" Also use spacebar as a leader
nmap <Space> \

nmap <Leader>* yiw/^<C-r><C-0>\s*:<CR>
nmap <Leader>/ /^\s*:\c<Left><Left><Left><Left><Left><Left>
nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>cdf :call Cdfile()<CR>
nnoremap <Leader>cmt :call CmtSection("")<Left><Left>
nnoremap <Leader>cx :!chmod a+x %<CR>
nnoremap <Leader>df :Gdiff<CR>
nnoremap <Leader>dom :Gdiff origin/master<CR>
nnoremap <Leader>ff :FSHere<CR>
nnoremap <Leader>fh :FSSplitLeft<CR>
nnoremap <Leader>fj :FSSplitBelow<CR>
nnoremap <Leader>fk :FSSplitAbove<CR>
nnoremap <Leader>fl :FSSplitRight<CR>
nnoremap <Leader>fx :call GTestFixture("")<Left><Left>
nnoremap <Leader>gg :Grepper<CR>
nnoremap <Leader>h :set hlsearch! hlsearch?<CR>
nnoremap <Leader>ia O// Application Includes<ESC>
nnoremap <Leader>ib O// BDE Includes<ESC>
nnoremap <Leader>ii O// Application Includes<CR>// BDE Includes<CR>// System Includes<ESC>
nnoremap <Leader>is O// System Includes<ESC>
nnoremap <Leader>jc :YcmCompleter GoToDeclaration<CR>
nnoremap <Leader>jf :YcmCompleter GoToDefinition<CR>
nnoremap <Leader>ji :YcmCompleter GoToImprecise<CR>
nnoremap <Leader>jj :YcmCompleter GoTo<CR>
nnoremap <Leader>pd <ESC>Opdb.set_trace()<ESC>j
nnoremap <Leader>rr :w<CR>:call VimuxRunCommand('./' . bufname("%"))<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>
nnoremap <Leader>ss :call SortSection()<CR>
nnoremap <Leader>te :tabe %:h<CR>
nnoremap <Leader>tf :call Cdfile()<CR>:call MkGtest()<CR>
nnoremap <Leader>tm :Tabmerge right<CR>
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>
nnoremap <Leader>vl :w<CR>:call VimuxRunCommand('(load "' . bufname("%") . '")')<CR>
nnoremap <Leader>vr :w<CR>:VimuxRunLastCommand<CR>
nnoremap <Leader>vt :w<CR>:call VimuxRunCommand("clear; LOCUM_LOCAL=1 gmake gtest")<CR>
nnoremap <Leader>vv :w<CR>:VimuxPromptCommand<CR>
nnoremap <Leader>vx :VimuxInterruptRunner<CR>
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>

" Local Leaders
autocmd FileType haskell nmap <buffer> <LocalLeader>sc :HoogleClose<CR>
autocmd FileType haskell nmap <buffer> <LocalLeader>sh :Hoogle
autocmd FileType haskell nmap <buffer> <LocalLeader>si :HoogleInfo<CR>

autocmd FileType haskell nmap <buffer> <F1> :HdevtoolsType<CR>
autocmd FileType haskell nmap <buffer> <F2> :HdevtoolsClear<CR>
autocmd FileType haskell nmap <buffer> <F3> :HdevtoolsInfo<CR>

if(g:bbenv != "")
    source ~/.vim/bundle/bde_plugins/bde_format.vim
endif

" Note - l and q are used for the location list and quickfix toggle by ListToggle
