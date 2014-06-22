" vim functions

function! StripTrailingWhitespaces()
  let _s=@/
  retab
  %s/\s\+$//e
  let @/=_s
  exec "normal ``"
endfunction

" Get OS Platform
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
    endif
endfunction

function! Cdscraper()
    cd ~/mbig/scrape.git/msgscrape/scraper
    pwd
endfunction

function! Cdgit()
    cd ~/mbig/scrape.git
    pwd
endfunction

function! Cdfile()
    cd %:h
    pwd
endfunction
