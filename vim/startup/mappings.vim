"" ============================================================================
""                             All Mode Mappings
"" ============================================================================
" Bael log category at the start of function
map <F6> <ESC>[[oBAEL_LOG_SET_CATEGORY(LOG_CATEGORY);<ESC><C-o>

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" =============================================================================
"                             Fix Tmux Mappings
" =============================================================================
if &term =~ '^screen' && exists('$TMUX')
    set mouse+=a
    " tmux knows the extended mouse mode
    set ttymouse=xterm2
    " tmux will send xterm-style keys when xterm-keys is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
    execute "set <xHome>=\e[1;*H"
    execute "set <xEnd>=\e[1;*F"
    execute "set <Insert>=\e[2;*~"
    execute "set <Delete>=\e[3;*~"
    execute "set <PageUp>=\e[5;*~"
    execute "set <PageDown>=\e[6;*~"
    execute "set <xF1>=\e[1;*P"
    execute "set <xF2>=\e[1;*Q"
    execute "set <xF3>=\e[1;*R"
    execute "set <xF4>=\e[1;*S"
    execute "set <F5>=\e[15;*~"
    execute "set <F6>=\e[17;*~"
    execute "set <F7>=\e[18;*~"
    execute "set <F8>=\e[19;*~"
    execute "set <F9>=\e[20;*~"
    execute "set <F10>=\e[21;*~"
    execute "set <F11>=\e[23;*~"
    execute "set <F12>=\e[24;*~"
endif

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

" Compilation and testing
nnoremap <F2> :w<CR>:call Cdroot()<CR>:call VimuxRunCommand("clear; make clean -j")<CR>
if(g:bbenv == "")
    nnoremap <F4> :call Cdroot()<CR>:Make all<CR>:Make run<CR>:Copen<CR>/FAILED<CR>
else
    nnoremap <F3> :w<CR>:call Cdroot()<CR>:call VimuxRunCommand("clear; make gtest -j")<CR>
    nnoremap <F4> :w<CR>:call Cdroot()<CR>:call VimuxRunCommand("clear; make -j")<CR>
endif

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
nnoremap <Leader>vl :w<CR>:call VimuxRunCommand('(load "' . bufname("%") . '")')<CR>
nnoremap <Leader>vr :w<CR>:VimuxRunLastCommand<CR>
nnoremap <Leader>vt :w<CR>:call VimuxRunCommand("clear; make test -j")<CR>
nnoremap <Leader>vv :w<CR>:VimuxPromptCommand<CR>
nnoremap <Leader>vx :VimuxInterruptRunner<CR>
nnoremap <Leader>w :call StripTabsAndTrailingWhitespaces()<CR>:w<CR>

" Note - l and q are used for the location list and quickfix toggle by ListToggle
nnoremap <Leader>p :pclose<CR>
