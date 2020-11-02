#!/bin/bash

#    Simple script to take picture after entering wrong password in Linux
#
#    Requirements:
#    Install ffmpeg or avconv
#    apt-get install libav-tools
#    (at your option) any later version.
#
#    blackMORE Ops <www.blackmoreops.com>

ts=`date +%s`
ffmpeg -f video4linux2 -s vga -i /dev/video0 -vframes 3 /tmp/vid-$ts.%01d.jpg
exit 0
#important - has to exit with status 0
