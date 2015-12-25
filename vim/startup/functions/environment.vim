"" ============================================================================
""                                Environment
"" ============================================================================
function! GetPlatform()
    if has('win32') || has('win64')
        return "Windows"
    elseif has("unix")
        return substitute(system("uname"), "\n", "", "g")
    else
        return "Unknown"
    endif
endfunction

function! GetBBENV()
    if has("unix")
        return substitute(system("echo $BBENV"), "\n", "", "g")
    else
        return ""
    endif
endfunction


