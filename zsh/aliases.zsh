if [ $(uname) = "Darwin" ]; then
    # if no gls, brew intall coreutils
    alias ls='gls --color=auto'
else
    alias ls='ls --color=auto'
fi

alias ai='sudo apt-get install'
alias cdr='cd $(git rev-parse --show-toplevel)'
alias fn='find . -name'
alias g='git'
alias gg='git grep'
alias ggi='git grep -i'
alias gm='gmake -j'
alias gmc='gmake -j clean'
alias gmr='gmake clean -j && gmake -j'
alias gmt='gmake test -j'
alias grep='grep --color=auto'
alias hd='~/bin/hex_decimal.sh'
alias less='less -N'
alias ll='ls -al'
alias lower="tr '[:upper:]' '[:lower:]'"
alias m='make -j'
alias mkcd='. ~/bin/make_dir_and_cd.sh'
alias mt='make -j test'
alias myip='curl cmyip.com 2>/dev/null | grep -o "My IP Address is [0-9.]*"'
alias p='ps aux | grep '
alias rfwifi='nmcli r wifi off && nmcli r wifi on'
alias tmux='tmux -2u'
alias topcpu='/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10'
alias upper="tr '[:lower:]' '[:upper:]'"
alias uu='sudo apt-get update && sudo apt-get upgrade'
alias wfc='curl "wttr.in/nyc?m"'

hash hub > /dev/null 2>&1
if [[ $? -eq 0 ]]; then
    alias git='hub'
    alias g='hub'
fi

[ -f ~/.zsh_local/aliases_local.zsh ] && source ~/.zsh_local/aliases_local.zsh

