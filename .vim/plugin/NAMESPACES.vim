
" Timer-saver for creating namespace blocks
function! DefNamespace(innerName)
   let curLine = getline('.')

   let str = "namespace BloombergLP {\n"
   let str = str . "namespace " . a:innerName . " {\n"
   let str = str . "\n\n\n"
   let str = str . "} // end namespace " . a:innerName . "\n"
   let str = str . "} // end namespace BloombergLP"
   put=str

   let curLine = curLine + 4
   execute "normal " . curLine . "gg"

endfunction
