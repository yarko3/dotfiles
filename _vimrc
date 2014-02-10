set nocompatible
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

" Set Color Scheme
:colorscheme ir_black

" Set <Space> + Character to insert 1 character, then go back to command mode
:nmap <Space> i_<Esc>r
