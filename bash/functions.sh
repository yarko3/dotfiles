# =============================================================================
#                                 Functions
# =============================================================================

# git commit browser
fshow() {
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


# load local functions
[ -f ~/.bashrc_local/bashrc_local_functions.sh ] && source ~/.bashrc_local/bashrc_local_functions.sh
