#!/bin/bash
# To create these profiles, run:
#    $ firefox -ProfileManager

if [ "$DISPLAY" == ':0.0' ]; then
    nohup firefox -no-remote -P BottomCenter &

elif [ "$DISPLAY" == ':0.1' ]; then
    nohup firefox -no-remote -P TopCenter &

elif [ "$DISPLAY" == ':0.2' ]; then
    nohup firefox -no-remote -P TopLeft &

elif [ "$DISPLAY" == ':0.3' ]; then
    nohup firefox -no-remote -P BottomLeft &

elif [ "$DISPLAY" == ':0.4' ]; then
    nohup firefox -no-remote -P BottomRight &

elif [ "$DISPLAY" == ':0.5' ]; then
    nohup firefox -no-remote -P TopRight &
fi
