#!/usr/bin/env bash
# calculation:
# 210 / (math.sqrt(1920**2 + 1080**2) / 14)
#
# Scren name     | Resolution | Size | DPI | Scaling          |
# ============================================================|
# Laptop: eDP1   | 1920x1080  | 14"  | 157 | 210/157 = 1.33   |
# Dell-2: DP-2-2 | 1920x1080  | 23"  | 96  | 210/96  = 2.187  |
# Dell-1 DP-2-3  | 1920x1080  | 27"  | 82  | 210/82  = 2.56   |
# ============================================================|

# had to downgrade DP-2-3 and set a lower resolution cause otherwise I got the
# following error:  'xrandr: Configure crtc 2 failed'
xrandr --output eDP-1 --auto --right-of DP-2-3 --output DP-2-3 --mode 1920x1080 --primary --right-of DP-2-2 --output DP-2-2 --auto --rotate left
