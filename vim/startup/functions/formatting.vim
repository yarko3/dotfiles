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
  let l:pos = getcurpos()
  let _s=@/
  retab
  %s///ge
  %s/\s\+$//e
  let @/=_s
  call setpos('.', l:pos)
endfunction

function! RightPad(str, padSequence, desiredLen)
    let padded = a:str
    let ct = len(a:str)
    while ct < a:desiredLen
        let padded = padded . a:padSequence
        let ct += 1
    endwhile
    return padded
endfunction

" Optional argument forces a specific comment string; otherwise, uses vim's
" commentstring variable
function! CmtSection(title, ...)
    let commentChar = split(&commentstring, "%s")[0]
    if(a:0 == 1)
        let commentChar = a:1
    endif

    put!=s:CmtSection(a:title, commentChar)
endfunction

function! s:CmtSection(title, commentStr)
    let str = RightPad(a:commentStr . " ", "=", 79) . "\n"

    let startCol = s:CenteredStringStartColumn(a:title) - strlen(a:commentStr) - 1
    let str = str . RightPad(a:commentStr, " ", startCol)

    let str = str . a:title . "\n"
    let str = str . RightPad(a:commentStr . " ", "=", 79)
    return str
endfunction

" Find and return a list of [namespace string, line number] pairs
function! FindNamespaces()
    let curLine = 0
    let namespaces = []

    while(curLine < line('$'))
        if(getline(curLine) =~# '^namespace \w* \={')
            let namespaceParts = split(getline(curLine))
            if(len(namespaceParts) == 2)
                let nsName = "anonymous"
            else
                let nsName = namespaceParts[1]
            endif

            let namespaces += [[nsName, curLine]]
        endif
        let curLine += 1
    endwhile

    return namespaces
endfunction

function! s:CenteredStringStartColumn(str)
    if strlen(a:str) >= 79
        return 0
    endif

    let midCol = 40
    let strMidptDist = strlen(a:str) / 2
    return midCol - strMidptDist
endfunction

function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"
    let l:pattern = escape(@", "\\/.*'$^~[]")
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'gv'
        call CmdLine("Ack '" . l:pattern . "' " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction
