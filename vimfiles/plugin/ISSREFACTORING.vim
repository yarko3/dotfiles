" ISSREFACTORING.vim
"	Author:	    Ben Hipple
"	Date:       23 January 2014
"	Updated:    24 January 2014
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


" Put the cursor on the first line of a function definition, and call this
" script.  It will add a standard format comment header to the function,
" putting the function name, return type, and all parameters into the
" comments
function AddFunctionHeader()
    " Get Function Information
    let funcLine = getline('.')
    let returnType = matchstr(funcLine, '^\S*') . matchstr(funcLine, '^\S*\s*\zs\**')
    let funcName = matchstr(funcLine, '^\S*\s*\zs[^(]*')

    " Get parameters on the function definition line
    let paramSection = matchstr(funcLine, '(\zs[^)]*')
    let paramSection = substitute(paramSection, '\s*,\s*', ',', 'g')
    let paramList = split(paramSection, ',')

    let i = line('.')
    let i = i + 1
    let paramLine = getline(i)

    " Get paramenters on following lines, up to the {
    while(matchstr(paramLine, '{') == "")
        let paramLine = matchstr(paramLine, '\s*\zs.*\ze[,\|)]')
        let paramLine = matchstr(paramLine, '\s*\zs\S*') . " " . matchstr(paramLine, '\s*\S*\s*\zs\S*')
        let paramList += [paramLine]
        let i = i + 1
        let paramLine = getline(i)
    endwhile

    " Build the function header
    let str = BuildFunctionHeader(funcName, returnType, paramList)
    :put! = str

endfunction


function BuildFunctionHeader(funcName, returnType, paramList)
    let str = '/**********************************************************************************************************'
    let str = str . "\n" . '*'
    let str = str . "\n" . '* NAME: ' . a:funcName
    let str = str . "\n" . '*'
    let str = str . "\n" . '* DESCRIPTION:'
    let str = str . "\n" . '*'
    let str = str . "\n" . '* INPUTS:'
    let str = str . "\n" . '*  Parameters Passed:'
    let str = str . "\n" . '*  ----------------------------'
    for param in a:paramList
        let str = str . "\n" . '*  ' . param
    endfor

    let str = str . "\n" . '*'
    let str = str . "\n" . '*  Globals Accessed:'
    let str = str . "\n" . '*  ----------------------------'
    let str = str . "\n" . '*'
    let str = str . "\n" . '* OUTPUTS:'
    let str = str . "\n" . '*  Value(s) Returned:'
    let str = str . "\n" . '*  ----------------------------'
    let str = str . "\n" . '*  ' . a:returnType
    let str = str . "\n" . '*'
    let str = str . "\n" . '*  Global(s) Modified:'
    let str = str . "\n" . '*  ----------------------------'
    let str = str . "\n" . '*'
    let str = str . "\n" . '* NOTES:'
    let str = str . "\n" . '*'
    let str = str . "\n" . '**********************************************************************************************************/'
    
    return str
endfunction


function! Trim(input_string)
    return substitute(a:input_string, '^\s*\(.\{-}\)\s*$', '\1', '')
endfunction
