#!/bin/bash

if [ "$RAN_STARTUP" || "$BBENV" ]
then
    return 0
fi
export RAN_STARTUP="TRUE"

# Open programs on monitors
DISPLAY=:0.1
nohup konsole

DISPLAY=:0.2
nohup konsole

DISPLAY=:0.4
nohup konsole

DISPLAY=:0.5
nohup konsole

DISPLAY=:0.3
nohup konsole
nohup skype


DISPLAY=:0.0
