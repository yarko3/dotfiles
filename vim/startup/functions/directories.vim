function! Cdfile()
    cd %:h
    pwd
endfunction

" cd to the root of the current file's git directory
function! Cdroot()
    cd %:h
    exec "cd " . Trim(system("git rev-parse --show-toplevel"))
    pwd
endfunction
