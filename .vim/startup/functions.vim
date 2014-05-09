" vim functions

function! StripTrailingWhitespaces()
  let _s=@/
  %s/\s\+$//e
  let @/=_s
  exec "normal ``"
endfunction
