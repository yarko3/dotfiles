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

pathDeduplicate() {
  export PATH="$(echo "$PATH" |
    awk 'BEGIN{RS=":";}
  {sub(sprintf("%c$",10),"");if(A[$0]){}else{A[$0]=1;printf(((NR==1)?"":":")$0)}}' \
    )";
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

# String to be used in window name that represents current VCS.
vcs_window_name() {
  git_branch_text="$(git symbolic-ref HEAD 2>/dev/null | cut -d'/' -f3)" || ""
  if [ -n "$git_branch_text" ]; then
    git_dir_text=$(basename "$(git rev-parse --show-toplevel)")
    prompt_text="$git_dir_text:$git_branch_text"
  else
    prompt_text=""
  fi

  # override with local, if exists
  if [[ -n $(whence local_vcs_name) ]]; then
    prompt_text_local="$(local_vcs_name)"
    if [[ -n $prompt_text_local ]]; then
      prompt_text=$prompt_text_local
    fi
  fi
  echo "$prompt_text"
}

# String to be used in prompt that represents current VCS.
vcs_prompt_name() {
  prompt_text="$(git symbolic-ref HEAD 2>/dev/null | cut -d'/' -f3)" || ""

  # override with local, if exists
  if [[ -n $(whence local_vcs_name) ]]; then
    prompt_text_local="$(local_vcs_name)"
    if [[ -n $prompt_text_local ]]; then
      prompt_text=$prompt_text_local
    fi
  fi
  echo "$prompt_text"
}

# If terminal in tmux, rename window based on current working directory.
rename_tmux_window() {
  # rename tmux window if we're in tmux
  if [ -n "$TMUX" ]; then
    tmux_name="$(vcs_window_name 2>/dev/null)"
    if [ -z "$tmux_name" ]; then
      tmux_name=$(basename "$(pwd)")
    fi
    tmux rename-window -t "$TMUX_WINDOW" "$tmux_name"
  fi
}

# load local functions
[ -f ~/.zsh_local/zshrc_local_functions.zsh ] && source ~/.zsh_local/zshrc_local_functions.zsh
