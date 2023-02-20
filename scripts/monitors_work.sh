#!/bin/bash

# Scren name     | Resolution | Size | DPI | Scaling          |
# ============================================================|
# Laptop: eDP1   | 2560x1440  | 14"  | 210 | 210/210 = 1x1    |
# Dell-1 DP-2-2  | 2560x1440  | 27"  | 109 | 210/109 = 1.926  |
# Dell-2: DP-2-1 | 1920x1080  | 23"  | 96  | 210/96  = 2.187  |
# ============================================================|

killall trayer
xrandr --output eDP-1 --auto --right-of DP-2-2 --output DP-2-2 --auto --primary --right-of DP-2-1 --output DP-2-1 --auto --rotate left
nohup trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --tint 0x222222 --height 41 --monitor 1 > /tmp/nohup.out 2>&1 &
rm -rf ~/.xmonad/xmonad.state

