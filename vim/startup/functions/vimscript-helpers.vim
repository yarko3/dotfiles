" Vimscript helpers
function! Trim(str)
    return substitute(a:str, '^\s*\(.\{-}\)\s*\n*$', '\1', '')
endfunction
