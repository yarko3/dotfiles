#!/usr/bin/env bash
# Requires the following packages:
# build-essential cmake python-dev
cmd='./install.py --clang-complete || echo "YCM Build failed. Are its dependencies installed?"'

cd ~/.vim/plugged/YouCompleteMe
if [ -f third_party/ycmd/ycm_core.so ]; then
    if [ "$1" == "-f" ]; then
        eval "$cmd"
    else
        echo "YCM already built. Use the -f flag to rebuild it."
    fi
else
    eval "$cmd"
fi
