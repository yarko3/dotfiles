#!/bin/bash
UPDATE_SESSION_NAME=updatePlugins

# start a server but don't attach to it
tmux start-server
# create a new session but don't attach to it either
tmux new-session -d -s $UPDATE_SESSION_NAME
# install the plugins
~/.tmux/plugins/tpm/scripts/install_plugins.sh
# update plugins
~/.tmux/plugins/tpm/bin/update_plugins all
# kill session
tmux kill-session -t $UPDATE_SESSION_NAME
