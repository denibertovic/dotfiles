#!/bin/bash

killall trayer
xrandr --output eDP-1 --auto --primary --below DP-2-2 --output DP-2-2 --right-of DP-2-1 --auto --output DP-2-1 --rotate left --auto
nohup trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --tint 0x222222 --height 33 --monitor 2 > /tmp/nohup.out 2>&1 &
rm -rf ~/.xmonad/xmonad.state

