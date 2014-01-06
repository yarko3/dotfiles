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
	:%s/active_fpl\./pRoute->/g
	:%s/&active_fpl/pRoute/g
	:%s/sets\./pSets->/g
	:%s/&sets/pSets/g
	:%s/active_legs\[\(.\{-}\)\]\./pLegs\[\1\]->/g
	:%s/&active_legs/pLegs/g
	:%s/active_vnav_legs\./pVnavLegs->/g
	:%s/&active_vnav_legs/pVnavLegs/g
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
function ChangeDirectory()
	:cd C:\CODE\BenRouteWorkspace\7H-09660_FMSGRP_OFP\fms\Source\
endfunction
