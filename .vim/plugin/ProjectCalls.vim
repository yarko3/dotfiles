" PROJECTCALLS.vim
"	Author:	    Ben Hipple
"	Date:       23 January 2014
"	Updated:    23 January 2014
"	Typical Usage:
"       For a particular function, use ProjectCall() to call it on all modules
"       in the project


" Calls functionName() on all *.c|h modules in the PWD and all subdirectories,
" and pipes output to a new buffer
function ProjectCall(functionName)
    redir => message
        :args **/*.h | :silent argdo execute ":call " . a:functionName . '()' | silent update
        :args **/*.c | :silent argdo execute ":call " . a:functionName . '()' | silent update
    redir END
    tabnew
    silent put=message
    set nomodified
endfunction

" Call this function on all code modules in the directory
" Dependency: REMOVEDEADBLOCKS.vim Plugin
function ProjectRemoveDeadBlocks()
    :call ProjectCall("RemoveDeadBlocks")
endfunction

" Project-Level refactoring, for starting a fresh merge
" Change your current working directory FIRST, before calling this function!
" Dependency: ISSREFACTORING.vim Plugin
function ProjectRefactor()
    :call ProjectCall("IssRefactoring")
endfunction

" Remove all lines that match [rf][ptr]
" Dependency: ISSREFACTORING.vim Plugin
function ProjectRipRfTags()
    :call ProjectCall("RipRfTags")
endfunction
