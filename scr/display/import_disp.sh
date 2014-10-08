#!/bin/bash
echo -e "Previous:\t$DISPLAY"
disp=$(cat ~/scr/display/display.txt)
export DISPLAY=$disp

echo -e "Exported:\t$disp"
