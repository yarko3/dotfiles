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

function! CmtSection(title, ...)
    let commentChar = "/"
    if(a:0 == 1)
        let commentChar = a:1
    endif

    put!=s:CmtSection(a:title, commentChar)
endfunction

function! s:CmtSection(title, commentChar)
    let str = a:commentChar . a:commentChar . " ============================================================================\n"
    let str = str . a:commentChar . a:commentChar . " "

    let startCol = s:CenteredStringStartColumn(a:title) - strlen("// ") - 1
    let ct = 0
    while ct < startCol
        let str = str . " "
        let ct += 1
    endwhile

    let str = str . a:title . "\n"
    let str = str . a:commentChar . a:commentChar . " ============================================================================"
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

function! FixIncludeGuard()
    " Only operate on header files
    if(expand('%:e') != 'h')
        return
    endif

    let correctGuard = 'INCLUDED_' . toupper(expand('%:t:r'))

    let curLine = 0
    let found = 0
    while(!found && curLine < line('$'))
        if(getline(curLine) =~# '^#ifndef \(INCLUDED_[A-Z_]\)')
            let incorrectGuard = (split(getline(curLine)))[1]
            exec '%s/' . incorrectGuard . '/' . correctGuard . '/ge'
            let found = 1
        endif
        let curLine += 1
    endwhile

    " BDE standard specify that #endif must not be followed by a comment
    %s/^#endif.*$/#endif/ge
endfunction

