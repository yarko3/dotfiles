#!/bin/bash
## ============================================================================
##                         Open Programs On Monitors
## ============================================================================
# Top Center
DISPLAY=:0.1
konsole &
~/bin/firefox_launch.sh &

# Top Left
DISPLAY=:0.2
konsole &
~/bin/firefox_launch.sh &

# Bottom Right
DISPLAY=:0.4
konsole &
~/bin/firefox_launch.sh &

# Top Right
DISPLAY=:0.5
konsole &
~/bin/firefox_launch.sh &

# Bottom Left
DISPLAY=:0.3
konsole &
skype &

# Bottom Center
DISPLAY=:0.0
~/bin/firefox_launch.sh &
