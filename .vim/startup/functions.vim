" vim functions

function! StripTabsAndTrailingWhitespaces()
  let _s=@/
  retab
  %s/\s\+$//e
  let @/=_s
  exec "normal ``"
endfunction

function! CmtSection(title)
    let str = "// ============================================================================\n"
    let str = str . "// "

    let startCol = XH_CenteredStringStartColumn(a:title) - strlen("// ") - 1
    let ct = 0
    while ct < startCol
        let str = str . " "
        let ct += 1
    endwhile

    let str = str . a:title . "\n"
    let str = str . "// ============================================================================"
    put!=str
endfunction

function! XH_CenteredStringStartColumn(str)
    if strlen(a:str) >= 79
        return 0
    endif

    let midCol = 40
    let strMidptDist = strlen(a:str) / 2
    return midCol - strMidptDist
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
    else
        return ""
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
