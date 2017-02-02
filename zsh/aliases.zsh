alias battery='upower -i /org/freedesktop/UPower/devices/battery_BAT0'
alias cdr='cd $(git rev-parse --show-toplevel)'
alias ff='firefox'
alias fn='find . -name'
alias g='hub'
alias gg='git grep'
alias ggi='git grep -i'
alias git='hub'
alias gm='gmake -j'
alias gmc='gmake -j clean'
alias gmr='gmake clean -j && gmake -j'
alias gmt='gmake test -j'
alias hd='~/bin/hex_decimal.sh'
alias less='less -N'
alias ll='ls -al'
alias lower="tr '[:upper:]' '[:lower:]'"
alias ls='ls --color'
alias m='make -j'
alias mkcd='. ~/bin/make_dir_and_cd.sh'
alias mt='make -j test'
alias myip='curl cmyip.com 2>/dev/null | grep -o "My IP Address is [0-9.]*"'
alias rfwifi='nmcli r wifi off && nmcli r wifi on'
alias tmux='tmux -2u'
alias upper="tr '[:lower:]' '[:upper:]'"
alias uu='sudo apt-get update && sudo apt-get upgrade'
alias wfc='weather -f NYC'

# Search running processes
alias p='ps aux | grep '
alias topcpu='/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10'

[ -f ~/.zsh_local/aliases_local.zsh ] && source ~/.zsh_local/aliases_local.zsh

