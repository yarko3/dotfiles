#!/bin/bash

if [ "$RAN_STARTUP" || "$BBENV" ]
then
    return 0
fi
export RAN_STARTUP="TRUE"

#==============================================================================
# Open programs on monitors
#
# Top Center
DISPLAY=:0.1
nohup konsole &
nohup firefox &

# Top Right
DISPLAY=:0.2
nohup konsole &
nohup filezilla &

# Bottom Right
DISPLAY=:0.4
nohup konsole &
nohup chromium-browser &

# Top Right
DISPLAY=:0.5
nohup konsole &
nohup keepass2 &

# Bottom Left
DISPLAY=:0.3
nohup konsole &
nohup skype &
nohup opera &

# Bottom Center
DISPLAY=:0.0
nohup google-chrome &
