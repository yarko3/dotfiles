" Mappings to Load


" Set <Space> + Character to insert 1 character, then go back to command mode
nmap <Space> i_<Esc>r

" Auto-Insertion for closing braces
inoremap {      {}<Left>
inoremap {<CR>  {<CR>}<Esc>O
inoremap {{     {
inoremap {}     {}

" Bael Log
inoremap <F2> BAEL_LOG_DEBUG << 
inoremap <F3> BAEL_LOG_ERROR << 
inoremap <F4> BAEL_LOG_END;<CR>

" Compilation
map <F3> :Make!<CR>
map <F4> :!make run<CR>

" Swap between cpp/h file
map <silent> <F8> :exec ":e ".(expand("%") =~ ".h$"
            \       ? glob(substitute(expand("%"), ".h$", ".cpp", ""))
            \          : substitute(expand("%"), "\\.cpp$", ".h", ""))<CR>

" Swap to last buffer
map <silent> <F9> :b#<CR>
