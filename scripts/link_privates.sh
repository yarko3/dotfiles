#!/bin/sh

PRIVATE_BIN="$HOME/private_bin"
BIN="$HOME/bin"

# link private executables
if [ -d "$PRIVATE_BIN" ] && [ -d "$BIN" ]; then
    ln -s "$PRIVATE_BIN"/* "$BIN"
fi
