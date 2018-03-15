#!/bin/bash

# Refresh environment variables from tmux.
if [ -n "$TMUX" ]; then
  sshauth=$(tmux show-environment | grep "^SSH_AUTH_SOCK")
  if [ "$sshauth" ]; then
    export $sshauth
  fi
  display=$(tmux show-environment | grep "^DISPLAY")
  if [ "$display" ]; then
    export $display
  fi
fi
