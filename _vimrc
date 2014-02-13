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
