if [ "$(uname)" = "Darwin" ]; then
    # if no gls, brew install coreutils
    alias ls='gls --color=auto'
else
    alias ls='ls --color=auto'
fi

alias cdr='cd $(git rev-parse --show-toplevel)'
alias fn='find . -name'
alias g='git'
alias gg='git grep'
alias ggi='git grep -i'
alias grep='grep --color=auto'
alias less='less -N'
alias ll='ls -lrtah'
alias lower="tr '[:upper:]' '[:lower:]'"
alias m='make -j'
alias mt='make -j test'
alias p='ps aux | grep '
alias tmux='tmux -2u'
alias topcpu='/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10'
alias upper="tr '[:lower:]' '[:upper:]'"
alias wfc='curl "wttr.in/nyc?m"'

[ -f ~/.zsh_local/zshrc_local_aliases.zsh ] && source ~/.zsh_local/zshrc_local_aliases.zsh

