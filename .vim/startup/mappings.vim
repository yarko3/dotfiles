" Mappings to Load


" Set <Space> + Character to insert 1 character, then go back to command mode
nmap <Space> i_<Esc>r

" Auto-Insertion for closing braces
inoremap {      {}<Left>
inoremap {<CR>  {<CR>}<Esc>O
inoremap {{     {
inoremap {}     {}

map <F3> :Make!<CR>
map <F4> :make<CR>
map <F5> :!./*.tsk<CR>

" Hit F8 to switch between cpp/h file
map <silent> <F8> :exec ":e ".(expand("%") =~ ".h$"
            \       ? glob(substitute(expand("%"), ".h$", ".cpp", ""))
            \          : substitute(expand("%"), "\\.cpp$", ".h", ""))<CR>

