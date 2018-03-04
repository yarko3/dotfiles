# git commit browser
function glg() {
  git log --graph --oneline --branches --decorate --color=always \
      --format=format:'%C(bold blue)%h%C(reset) - %C(bold blue)(%ar) %C(bold yellow)%d%C(reset) %C(green)%s%C(reset) %C(dim green)- %an%C(reset)' "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

function noproxy() {
    env -u http_proxy -u https_proxy -u HTTP_PROXY -u HTTPS_PROXY "$@"
}

pathDeduplicate() {
    export PATH="$(echo "$PATH" |
        awk 'BEGIN{RS=":";}
            {sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}' \
        )";
}

function logview {
    if (( $# == 3 )); then
        ls -rt $1/$2.log* | tail -$3 | head -1
    elif (( $# == 2 )); then
        less -n -S $(ls -rt $1/$2.log* | tail -1)
    else
        less -n -S $(ls -rt ${LOCAL_LOG_DIR}/$1.log* | tail -1)
    fi
}

unalias z 2> /dev/null
z() {
    [ $# -gt 0 ] && _z "$*" && return
    cd "$(_z -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "${*##-* }" | sed 's/^[0-9,.]* *//')"
}

function pager_wrapper () {
    if [[ -t 1 ]]; then
        "$@" | less -+c -FRX
    else
        "$@"
    fi
}

# find and kill a process
snipe () {
  pid=$(ps -ef | sed 1d | grep -v "fzf -m --query='$1" | fzf -m --query="'$1" | awk '{print $2}')
  if [ -n "$pid" ]
  then
    kill -9 "$pid"
  fi
}

# load local functions
[ -f ~/.zshrc_local/zshrc_local_functions.sh ] && source ~/.zshrc_local/zshrc_local_functions.sh
