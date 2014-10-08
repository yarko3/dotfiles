#!/bin/bash

cd ~/scr/display/
[ -f display.txt ] && echo "Previous display value: " && eval cat display.txt
rm -f display.txt

echo "$DISPLAY" >> display.txt
echo "New display value: "
eval cat display.txt
