#!/bin/bash
## ============================================================================
##                         Open Programs On Monitors
## ============================================================================
# Top Center
DISPLAY=:0.1
nohup konsole &
nohup ~/scr/x_server/firefox_launch.sh &

# Top Left
DISPLAY=:0.2
nohup konsole &
nohup filezilla &
nohup ~/scr/x_server/firefox_launch.sh &

# Bottom Right
DISPLAY=:0.4
nohup konsole &
nohup ~/scr/x_server/firefox_launch.sh &

# Top Right
DISPLAY=:0.5
nohup konsole &
nohup keepass2 &

# Bottom Left
DISPLAY=:0.3
nohup konsole &
nohup skype &
nohup ~/scr/x_server/firefox_launch.sh &

# Bottom Center
DISPLAY=:0.0
nohup ~/scr/x_server/firefox_launch.sh &
