function! Cdfile()
    if expand('%') != ''
        cd %:h
    else
        echom "Not currently in a file."
    endif
endfunction

" cd to the root of the current file's git directory
function! Cdroot()
    call Cdfile()
    exec "cd " . Trim(system("git rev-parse --show-toplevel"))
    echom expand('.')
endfunction
