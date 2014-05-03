" REMOVEDEADBLOCKS.vim
"	Author:	    Ben Hipple
"	Date:       22 January 2014
"	Updated:    23 January 2014
"	Typical Usage:
"	    Removes #if 0 blocks from the codebase to make it smaller, cleaner,
"	    and easier to read.  Leaves a comment with the date and number of
"	    lines removed, so that it is easy to find the removed lines in
"	    Surround.

if exists("loaded_REMOVEREMOVEDBLOCKS")
	finish
endif
let loaded_REMOVEREMOVEDBLOCKS=1

" Remove all #if 0 blocks in the current module
function! RemoveDeadBlocks()
    " Configuration Options
    let removeIfZero = 1
    let removeDeactivatedCode = 1

    let totalLinesDeleted = 0
    let thisBlockLinesDeleted = 1
    let i = 0
    let emb = 0
    " For all lines in the module
    while i<=line("$")
        let line=getline(i)

        if removeIfZero  == 1 && line =~ "^\s*\#if 0"
            let emb=emb+1
        elseif removeDeactivatedCode == 1 && line =~ "^\s*\#ifdef USE_DEACTIVATED_CODE"
            let emb=emb+1
        elseif emb > 0 && line =~ "^\s*#if"
            let emb=emb+1
        endif

        if line =~ "^\s*\#else"
            " If we're only 1 level deep and we see an else, that's the end of
            " the #if 0
            if emb == 1
                let newline=substitute(line,"else","if 1","")
                call setline(i,newline)
                let emb=emb-1
            endif

            " Otherwise, it doesn't impact our level depth
        endif

        " Only decrement the #endif counter if we're in an #if 0 block
        if line =~ "^\s*\#endif" && emb > 0
            " If we're at the end, delete the #endif that goes with the #if 0
            if emb == 1
                let replaceComment = '//[rf][rip] Removed a ' . thisBlockLinesDeleted . ' line #if 0 block on ' . strftime('%d %b %Y') . ''
                call setline(i,replaceComment)
                let totalLinesDeleted=totalLinesDeleted+1
                let thisBlockLinesDeleted = 1
            endif
            let emb=emb-1
        endif

        " In deletion mode (haven't yet hit a matching  #endif)
        if emb > 0
            execute i."d"
            let i=i-1
            let totalLinesDeleted=totalLinesDeleted+1
            let thisBlockLinesDeleted=thisBlockLinesDeleted+1
        endif

        let i=i+1
    endwhile

    if totalLinesDeleted > 0
        :echom "Removed" totalLinesDeleted "lines"
    endif
endfunction

