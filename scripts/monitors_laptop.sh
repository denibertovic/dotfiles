#!/bin/bash

killall trayer
# Workaround for dunst not refreshing it's geometry and positioning after
# switching back to a single monitor
killall dunst
xrandr --output eDP-1 --auto --primary --output DP-2-2 --off --output DP-2-1 --off
nohup trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 20 --transparent true --tint 0x222222 --height 41 > /tmp/nohup.out 2>&1 &
rm -rf ~/.xmonad/xmonad.state

