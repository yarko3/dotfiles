" ISSREFACTORING.vim
"	Author:	    Ben Hipple
"	Date:       23 January 2014
"	Updated:    23 January 2014
"	Typical Usage:
"       Replaces all instances of particular globals with consistently named
"       pointers to the sub-structures of a flightplan


" Replacing globals with pointers.  Use the function in PROJECTCALLS.vim to
" replace all of them at once (in all code modules)
function IssRefactoring()
    " Names for rte_struct pointers
    :%s/active_fpl\./pRoute->/ge
    :%s/&\=active_fpl/pRoute/ge
    :%s/pRte/pRoute/ge
    :%s/pFpln/pRoute/ge

    " Names for settings_struct pointers
    :%s/\([^vnav_]\)sets\./\1pSets->/ge        " Don't change the vnav_sets global (yet), since it's currently not defined
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

" Add pointers to the refactored global variable replacement.  Timesaver when
" editing a function that has been touched by IssRefactoring()
function IssPointers()
    put='    /* [rf][ptr] Direct pointers to sub-structs of the Flightplan used by this function */'
    put='    rte_struct *pRoute = &pFlightplan->rte;'
    put='    settings_struct *pSets = &pFlightplan->settings;'
    put='    leg_struct (*pLegs)[MAX_FPL_WPTS] = &pFlightplan->legs;'
    put='    vnav_leg_struct *pVnavLegs = &pFlightplan->vnav_legs;'
    put=''
endfunction

" Quick script to remove any comment line with [rf][ptr], if they are no
" longer wanted
function RipRfTags()
    :%s/^\[rf\]\[ptr\]$//g
endfunction

