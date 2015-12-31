#!/bin/bash
# Requires the following packages:
# build-essential cmake python-dev

cd ~/.vim/plugged/YouCompleteMe
if [ -f third_party/ycmd/ycm_core.so ]; then
    if [ "$1" == "-f" ]; then
        ./install.py --clang-completer
    else
        echo "YCM already built. Use the -f flag to rebuild it."
    fi
else
    ./install.py --clang-completer
fi

