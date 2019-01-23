if [ "$(uname)" = "Linux" ]; then
    alias ls='ls --color=auto'
fi

alias cdr='cd $(git rev-parse --show-toplevel)'
alias fn='find . -name'
alias g='git'
alias gg='git grep'
alias ggi='git grep -i'
alias grep='pager_wrapper grep --color=auto'
alias h='hg'
alias home='cd $HOME'
alias kf='kill -9'
alias less='less -N'
alias ll='ls -lrtah'
alias lower="tr '[:upper:]' '[:lower:]'"
alias m='make -j'
alias mt='make -j test'
alias p='ps --sort=start_time aux | grep '
alias t='tmux'
alias tmux='tmux -2u'
alias to_clipboard='xclip -selection c'
alias topcpu='/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10'
alias upper="tr '[:lower:]' '[:upper:]'"
alias v='vim'
alias wfc='curl "wttr.in/nyc?m"'

[ -f ~/.zsh_local/zshrc_local_aliases.zsh ] && source ~/.zsh_local/zshrc_local_aliases.zsh
