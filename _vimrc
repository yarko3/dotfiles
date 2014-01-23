set nocompatible
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

" Set Color Scheme
:colorscheme ir_black

" Tabs Automatically Inserted as Spaces
:set shiftwidth=4
:set tabstop=4
:set expandtab

" Show tab whitespace characters
:set listchars=tab:>-
:set list!

" Set <Space> + Character to insert 1 character, then go back to command mode
:nmap <Space> i_<Esc>r



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


