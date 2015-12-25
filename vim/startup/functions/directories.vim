" cd into directory with the makefile
function! MakefileSearch()
    cd %:h
    pwd
    let ct = 0

    while(ct < 5 && ((len(split(globpath('.', 'Makefile'), '\n')) == 0) && (len(split(globpath('.', 'GNUmakefile'), '\n')) == 0)))
        cd ..
        pwd
        let ct += 1
    endwhile
endfunction

function! Cdfile()
    cd %:h
    pwd
endfunction
