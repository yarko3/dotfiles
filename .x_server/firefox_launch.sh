#!/bin/bash
# To create these profiles, run:
#    $ firefox -ProfileManager


# Monitors
BottomCenter=0
TopCenter=1
TopLeft=2
BottomLeft=3
BottomRight=4
TopRight=5


if [ "$DISPLAY" == ':0.0' ]; then
    nohup firefox -no-remote -P $BottomCenter &

elif [ "$DISPLAY" == ':0.1' ]; then
    nohup firefox -no-remote -P $TopCenter &

elif [ "$DISPLAY" == ':0.2' ]; then
    nohup firefox -no-remote -P $TopLeft &

elif [ "$DISPLAY" == ':0.3' ]; then
    nohup firefox -no-remote -P $BottomLeft &

elif [ "$DISPLAY" == ':0.4' ]; then
    nohup firefox -no-remote -P $BottomRight &

elif [ "$DISPLAY" == ':0.5' ]; then
    nohup firefox -no-remote -P $TopRight &
fi
