## ============================================================================
##                                 Settings
## ============================================================================
# Vim mode
bindkey -v

bindkey -M "vicmd" 'k' history-substring-search-up
bindkey -M "vicmd" 'j' history-substring-search-down

# Run `bindkey -l` to see a list of modes, and `bindkey -M foo` to see a list of commands active in mode foo
# Move to vim escape mode
bindkey -M "viins" kj vi-cmd-mode
bindkey -M "viins" kk vi-cmd-mode

# Unmap ctrl-s as "stop flow"
stty stop undef

# Shift-tab to cycle backwards in autocompletions
bindkey '^[[Z' reverse-menu-complete

# don't autocorrect
unsetopt correctall

# Don't save duplicated entries into history
setopt hist_ignore_all_dups

#  ============================================================================
#                            Configure Plugins
#  ============================================================================
# zsh-autosuggestions
# Bind <CTRL><SPC> to accept
bindkey '^ ' autosuggest-accept

## ============================================================================
##                                  Prompt
## ============================================================================
vcs_prompt_info_mine() {
  vcs_prompt_name_text="$(vcs_prompt_name 2>/dev/null)"
  if [[ -n $vcs_prompt_name_text ]]; then
    echo " <$vcs_prompt_name_text>"
  else
    echo ""
  fi
  rename_tmux_window > /dev/null 2>&1
}
export PROMPT='%{$fg[green]%}%n@%m %{$fg[cyan]%}%c%{$fg[yellow]%}$(vcs_prompt_info_mine)%{$reset_color%} $ '

export ZSH_THEME_GIT_PROMPT_PREFIX=""
export ZSH_THEME_GIT_PROMPT_SUFFIX=""

#  ============================================================================
#                                 Cursor
#  ============================================================================
# ZLE hooks for prompt's vi mode status
function zle-line-init zle-keymap-select {
# Change the cursor style depending on keymap mode.
case $KEYMAP {
  vicmd)
    printf '\e[0 q' # Box.
    ;;

  viins|main)
    printf '\e[6 q' # Vertical bar.
    ;;
  }
}
zle -N zle-line-init
zle -N zle-keymap-select

#  ============================================================================
#                                   SSH
#  ============================================================================
# start agent
[ -z "$SSH_AUTH_SOCK" ] && eval "$(ssh-agent -s)"

#  ============================================================================
#                                   FZF
#  ============================================================================
if [ -f /usr/share/doc/fzf/examples/key-bindings.zsh ]; then
  source /usr/share/doc/fzf/examples/key-bindings.zsh
elif [ -f /usr/local/brew/opt/fzf/shell/key-bindings.zsh ]; then
  source /usr/local/brew/opt/fzf/shell/key-bindings.zsh
fi

if [ -f /usr/share/doc/fzf/examples/completion.zsh ]; then
  source /usr/share/doc/fzf/examples/completion.zsh
elif [ -f /usr/local/brew/opt/fzf/shell/completion.zsh ]; then
  source /usr/local/brew/opt/fzf/shell/completion.zsh
fi
