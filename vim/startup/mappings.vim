"" ============================================================================
""                             All Mode Mappings
"" ============================================================================
" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

"" ============================================================================
""                           Insert Mode Mappings
"" ============================================================================
" Leaving insert mode with some ups and downs
inoremap kj <Esc>

"" ============================================================================
""                        Normal/Visual Mode Mappings
"" ============================================================================
" If hl search is off, starting a new search or moving enables it
" remove zz if the jumps are too much
nnoremap <silent> * :set hlsearch<CR>*zz:ShowSearchIndex<CR>
nnoremap <silent> # :set hlsearch<CR>#zz:ShowSearchIndex<CR>
nnoremap <silent> n :set hlsearch<CR>nzz:ShowSearchIndex<CR>
nnoremap <silent> N :set hlsearch<CR>Nzz:ShowSearchIndex<CR>

" Homerow visual navigation
vnoremap H 0
vnoremap L $

" This makes j and k work on "screen lines" instead of on "file lines"; now, when
" we have a long line that wraps to multiple screen lines, j and k behave as we
" expect them to.
nnoremap j gj
nnoremap k gk

" enable hlsearch (and, implicitly, incsearch) when searching
nnoremap <silent> / :set hlsearch<CR>/
nnoremap <silent> ? :set hlsearch<CR>?

" Press * or # to search for the current selection
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>:set hlsearch<CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>:set hlsearch<CR>

" Swap to last buffer
nnoremap <silent> <F8> :b#<CR>

" yank from cursor to end of line (not nnoremap because highlighted yank)
nmap Y y$

" replay @q macro for each line of a visual selection
vnoremap Q :normal @q<CR>

" qq to record, Q to replay
nnoremap Q @q

" resize splits easily
nnoremap <silent> <Tab>h :<C-U>ObviousResizeLeft<CR>
nnoremap <silent> <Tab>j :<C-U>ObviousResizeDown<CR>
nnoremap <silent> <Tab>k :<C-U>ObviousResizeUp<CR>
nnoremap <silent> <Tab>l :<C-U>ObviousResizeRight<CR>

"" =============================================================================
""                           Command Mode Mappings
"" =============================================================================
" Now we don't have to move our fingers so far when we want to scroll through
" the command history; also, don't forget the q: command (see :h q: for more
" info)
cnoremap <c-j> <down>
cnoremap <c-k> <up>
cnoremap <c-h> <left>
cnoremap <c-l> <right>

"" ============================================================================
""                         Leader Mappings (Sorted)
"" ============================================================================
" use spacebar as a leader
let mapleader = "\<Space>"

nnoremap <Leader><Space> :CtrlPBuffer<CR>
nnoremap <Leader>=       mmgg=G`m
nnoremap <Leader>cmt     :call CmtSection("")<Left><Left>
nnoremap <Leader>cx      :!chmod a+x %<CR>
nnoremap <Leader>df      :SignifyDiff<CR>
nnoremap <Leader>doff    :windo diffoff<CR>
nnoremap <Leader>dom     :Gdiff origin/master<CR>
nnoremap <Leader>dt      :windo diffthis<CR>
nnoremap <Leader>dum     :Gdiff upstream/master<CR>
nnoremap <Leader>ew      <C-S-w>=
nnoremap <Leader>ex      :call VimuxRunCommand(expand('%:p'))<CR>
nnoremap <Leader>ff      :FSHere<CR>
nnoremap <Leader>fh      :FSSplitLeft<CR>
nnoremap <Leader>fj      :FSSplitBelow<CR>
nnoremap <Leader>fk      :FSSplitAbove<CR>
nnoremap <Leader>fl      :FSSplitRight<CR>
nnoremap <Leader>gg      :call CdRoot()<CR>:Grepper<CR>
nnoremap <Leader>h       :set hlsearch! hlsearch?<CR>
nnoremap <Leader>jf      :YcmCompleter FixIt<CR>
nnoremap <Leader>jj      :YcmCompleter GoTo<CR>
nnoremap <Leader>jk      :YcmCompleter GoToReferences<CR>
nnoremap <Leader>jl      :vsp<CR>:LspDefinition<CR>
nnoremap <Leader>jr      :YcmRestartServer<CR>
nnoremap <Leader>k       :VimuxPromptCommand<CR>
nnoremap <Leader>lg      :Glog<CR>
nnoremap <Leader>lt      :LengthmattersToggle<CR>
nnoremap <Leader>mi      :VimuxInspectRunner<CR>
nnoremap <Leader>purge   :set vi+='0<CR>:wv!<CR>
nnoremap <Leader>r       :call VimuxRunCommand("!! \t")<CR>
nnoremap <Leader>so      :so %<CR>
nnoremap <Leader>sp      :setlocal spell! spelllang=en_us<CR>
nnoremap <Leader>ss      :call SortSection()<CR>
nnoremap <Leader>st      :ALEToggle<CR>
nnoremap <Leader>sw      :call WindowSwap#EasyWindowSwap()<CR>
nnoremap <Leader>t       :TagbarToggle<CR>
nnoremap <Leader>ut      :UndotreeToggle<CR>
nnoremap <Leader>uu      :PlugUpgrade<CR>:PlugUpdate<CR>
nnoremap <Leader>vh      :abo vsp<CR>:e %:h<CR>
nnoremap <Leader>vj      :sp<CR>:e %:h<CR>
nnoremap <Leader>vk      :abo sp<CR>:e %:h<CR>
nnoremap <Leader>vl      :vsp<CR>:e %:h<CR>
nnoremap <Leader>vv      :e %:h<CR>
nnoremap <Leader>w       :call StripTabsAndTrailingWhitespaces()<CR>:w<CR>
nnoremap <Leader>x       :q<CR>
nnoremap <Leader>z       :MaximizerToggle<CR>

" =============================================================================
"                          <Plug> Leader Mappings
" =============================================================================
" jump around easier
nmap <Leader>e <Plug>(easymotion-overwin-f)

" align things from visual mode as well as a motion
xmap <Leader>ga <Plug>(EasyAlign)
nmap <Leader>ga <Plug>(EasyAlign)

" yank over SSH
map <Leader>y <Plug>(operator-poweryank-osc52)

"" =============================================================================
""                             Fix Tmux Mappings
"" =============================================================================
if &term =~ '^screen' && exists('$TMUX')
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
