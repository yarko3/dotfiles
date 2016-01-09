" Sort all lines until a blank is encountered
function! SortSection()
    let startLine = line('.')
    let curLine = startLine

    while(getline(curLine+1) != "")
        let curLine += 1
    endwhile

    call setline(startLine, sort(getline(startLine, curLine)))
endfunction

function! StripTabsAndTrailingWhitespaces()
  let _s=@/
  retab
  %s/\s\+$//e
  let @/=_s
  exec "normal ``"
endfunction
