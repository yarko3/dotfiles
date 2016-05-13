#!/bin/bash
uname -o | grep -q "Linux"
dir=$(pwd)
if [ $? -eq 0 ]; then
    ln -fs "$dir/lib/hub-linux" "$dir/bin/hub"
    echo "Linked hub for Linux"
fi
