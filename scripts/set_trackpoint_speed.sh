#!/usr/bin/env bash
#https://www.reddit.com/r/thinkpad/comments/z3gw6x/help_t480_trackpoint_is_awfully_slow_in_linux/
#xinput set-prop 12 187 X 0 0 0 Y 0 0 0 1
# xinput set-prop 11 187 2.2 0 0 0 2.2 0 0 0 1
xinput set-prop "TPPS/2 IBM TrackPoint" 187 2.0 0 0 0 2.0 0 0 0 1

# https://www.reddit.com/r/thinkpad/comments/b5gqsv/change_trackpoint_speed_under_linux/
# xinput --set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Speed" 1.00
# xinput --set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Profile Enabled" 0 1
#
# xinput --set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Speed" 0.80
# xinput --set-prop "TPPS/2 IBM TrackPoint" "libinput Accel Profile Enabled" 0 1
