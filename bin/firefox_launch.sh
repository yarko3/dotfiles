#!/bin/bash
# To create these profiles, run:
#    $ \firefox -ProfileManager
#
# Monitors
BottomCenter=0
TopCenter=1
TopLeft=2
BottomLeft=3
BottomRight=4
TopRight=5

if ! [ "$HOSTNAME" = "brh-desktop" ]; then
    echo "Launching standard Firefox"
    firefox &
    exit 0
fi

if [ "$DISPLAY" == ':0.0' ]; then
    echo "Launching Firefox profile $BottomCenter"
    firefox -no-remote -P $BottomCenter &

elif [ "$DISPLAY" == ':0.1' ]; then
    echo "Launching Firefox profile $TopCenter"
    firefox -no-remote -P $TopCenter &

elif [ "$DISPLAY" == ':0.2' ]; then
    echo "Launching Firefox profile $TopLeft"
    firefox -no-remote -P $TopLeft &

elif [ "$DISPLAY" == ':0.3' ]; then
    echo "Launching Firefox profile $BottomLeft"
    firefox -no-remote -P $BottomLeft &

elif [ "$DISPLAY" == ':0.4' ]; then
    echo "Launching Firefox profile $BottomRight"
    firefox -no-remote -P $BottomRight &

elif [ "$DISPLAY" == ':0.5' ]; then
    echo "Launching Firefox profile $TopRight"
    firefox -no-remote -P $TopRight &
fi
