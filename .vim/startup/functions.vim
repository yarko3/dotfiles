" vim functions

function! StripTrailingWhitespaces()
  let _s=@/
  %s/\s\+$//e
  let @/=_s
  exec "normal ``"
endfunction


" Get OS Platform
function GetPlatform()
    if has('win32') || has('win64')
        return "Windows"
    elseif has("unix")
        return substitute(system("uname"), "\n", "", "g")
    else
        return "Unknown"
    endif
endfunction
