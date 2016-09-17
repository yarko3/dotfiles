"" ============================================================================
""                             All Mode Mappings
"" ============================================================================
" Bael log category at the start of function
map <F6> <ESC>[[oBAEL_LOG_SET_CATEGORY(LOG_CATEGORY);<ESC><C-o>

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

"" ============================================================================
""                           Insert Mode Mappings
"" ============================================================================
" Leaving insert mode with jj or jk
inoremap jj <Esc><Right>
inoremap jk <Esc><Right>

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

" Swap to last buffer
nnoremap <silent> <F8> :b#<CR>

noremap <F5> :cex[]<CR>:cclose<CR>

" Compilation and testing with Dispatch
nnoremap <F2> :call Cdroot()<CR>:Make clean<CR>
if(g:bbenv == "")
    nnoremap <F4> :call Cdroot()<CR>:Make all<CR>:Make run<CR>:Copen<CR>/FAILED<CR>
else
    nnoremap <F4> :w<CR>:call Cdroot()<CR>:Make!<CR>
endif

" Shift + movement for selection
nmap <S-Up> v<Up>
nmap <S-Down> v<Down>
nmap <S-Left> v<Left>
nmap <S-Right> v<Right>
vmap <S-Up> <Up>
vmap <S-Down> <Down>
vmap <S-Left> <Left>
vmap <S-Right> <Right>

"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
" Also use spacebar as a leader
nmap <Space> \

nnoremap <Leader>b :CtrlPBuffer<CR>
nnoremap <Leader>cdf :call Cdfile()<CR>
nnoremap <Leader>cdr :call Cdroot()<CR>
nnoremap <Leader>cmt :call CmtSection("")<Left><Left>
nnoremap <Leader>cx :!chmod a+x %<CR>
nnoremap <Leader>df :Gdiff<CR>
nnoremap <Leader>dom :Gdiff origin/master<CR>
nnoremap <Leader>dum :Gdiff upstream/master<CR>
nnoremap <Leader>ev :e $MYVIMRC<CR>G$F/
nnoremap <Leader>ff :FSHere<CR>
nnoremap <Leader>fh :FSSplitLeft<CR>
nnoremap <Leader>fj :FSSplitBelow<CR>
nnoremap <Leader>fk :FSSplitAbove<CR>
nnoremap <Leader>fl :FSSplitRight<CR>
nnoremap <Leader>gg :call Cdroot()<CR>:Grepper<CR>
nnoremap <Leader>h :set hlsearch! hlsearch?<CR>
nnoremap <Leader>jc :YcmCompleter GoToDeclaration<CR>
nnoremap <Leader>jf :YcmCompleter GoToDefinition<CR>
nnoremap <Leader>ji :YcmCompleter GoToImprecise<CR>
nnoremap <Leader>jj :YcmCompleter GoTo<CR>
nnoremap <Leader>rd :redraw!<CR>
nnoremap <Leader>se :sp<CR>:e %:h<CR>
nnoremap <Leader>sp :setlocal spell! spelllang=en_us<CR>
nnoremap <Leader>ss :call SortSection()<CR>
nnoremap <Leader>te :tabe %:h<CR>
nnoremap <Leader>tf :call Cdroot()<CR>:call MkGtest()<CR>
nnoremap <Leader>tm :Tabmerge right<CR>
nnoremap <Leader>ve :vsp<CR>:e %:h<CR>
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>:w<CR>

" Note - l and q are used for the location list and quickfix toggle by ListToggle
nnoremap <Leader>p :pclose<CR>
