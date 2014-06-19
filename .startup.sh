#!/bin/bash
#==============================================================================
# Open programs on monitors
#
# Top Center
DISPLAY=:0.1
nohup konsole &
nohup ~/.firefox-x-launch.sh &

# Top Left
DISPLAY=:0.2
nohup konsole &
nohup steam &
nohup ~/.firefox-x-launch.sh &

# Bottom Right
DISPLAY=:0.4
nohup konsole &
nohup ~/.firefox-x-launch.sh &

# Top Right
DISPLAY=:0.5
nohup konsole &
nohup keepass2 &

# Bottom Left
DISPLAY=:0.3
nohup konsole &
nohup skype &
nohup ~/.firefox-x-launch.sh &

# Bottom Center
DISPLAY=:0.0
nohup ~/.firefox-x-launch.sh &
