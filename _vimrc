set nocompatible
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

" Set Color Scheme
:colorscheme ir_black

" Tabs as Spaces
:set shiftwidth=4
:set tabstop=4
:set expandtab

" Show tab whitespace characters
:set listchars=tab:>-
:set list!

" Set <Space> + Character to insert 1 character, then go back to command mode
:nmap <Space> i_<Esc>r


" Replacing globals with pointers
function IssRefactoring()
    " Names for rte_struct pointers
    :%s/active_fpl\./pRoute->/ge
    :%s/&\=active_fpl/pRoute/ge
    :%s/pRte/pRoute/ge
    :%s/pFpln/pRoute/ge

    " Names for settings_struct pointers
    :%s/\([^vnav_]\)sets\./\1pSets->/ge        " Don't change the vnav_sets global (yet)
    :%s/&sets/pSets/ge

    " Names for leg_struct pointers
    :%s/active_legs\[\(.\{-}\)\]\./pLegs\[\1\]->/ge
    :%s/active_legs\[\(.\{-}\)\]/\*pLegs\[\1\]/ge
    :%s/&active_legs/pLegs/ge

    " Names for vnav_leg_struct pointers
    :%s/active_vnav_legs\./pVnavLegs->/ge
    :%s/&active_vnav_legs/pVnavLegs/ge
    :%s/pVFpln/pVnavLegs/ge
    :%s/pVnav\(\A\)/pVnavLegs\1/ge       " Change pVnav[non-alphabetic character] to pVnavLegs[non-alphabetic character]
endfunction

" Project-Level refactoring, for starting a fresh merge (untested)
" Change your current working directory FIRST, before calling this function!
function IssProjectRefactor()
    :args **/*.c | :silent argdo execute ":call IssRefactoring()" | silent update
    :args **/*.h | :silent argdo execute ":call IssRefactoring()" | silent update
endfunction


" Add pointers to the refactored global variable replacement
function IssPointers()
    put='    /* [rf][ptr] Direct pointers to sub-structs of the Flightplan used by this function */'
    put='    rte_struct *pRoute = &pFlightplan->rte;'
    put='    settings_struct *pSets = &pFlightplan->settings;'
    put='    leg_struct (*pLegs)[MAX_FPL_WPTS] = &pFlightplan->legs;'
    put='    vnav_leg_struct *pVnavLegs = &pFlightplan->vnav_legs;'
    put=''
endfunction


" Timesaver
function CDoldroute()
    :cd C:\CODE\BenRouteWorkspace\7H-09660_FMSGRP_OFP\fms\Source\
endfunction


function CDroute()
    :cd C:\CODE\AN-MCDU_Route_Update\7H-09660_FMSGRP_OFP\fms\Source\
endfunction


function CDiop()
    :cd C:\CODE\B737\7H-88016_FMS\7H-87012_ANMCDU\7H-09663_IOP_OFP\app
endfunction


function CDfmsgrp()
    :cd C:\CODE\B737\7H-88016_FMS\7H-87012_ANMCDU\7H-09660_FMSGRP_OFP\fms\Source
endfunction


function CDifpd()
    :cd C:\CODE\B737\7H-88013_FPDS\7H-84137_IFPD
endfunction
