#!/bin/bash
disp=$(cat ~/scr/display/display.txt)
export DISPLAY=$disp

eval echo "Set display to $(cat ~/scr/display/display.txt)"
