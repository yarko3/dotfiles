#!/bin/bash
cd ~/scr/display/
[ -f display.txt ] && echo -e "Previous:\t$(cat display.txt)"
rm -f display.txt

echo "$DISPLAY" >> display.txt
echo -e "display.txt:\t$(cat display.txt)"
