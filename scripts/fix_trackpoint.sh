#!/bin/bash

if [ "$EUID" -ne 0 ]
  then echo "Please run as root."
  exit
fi

echo 200 >  /sys/devices/platform/i8042/serio1/serio2/sensitivity
echo 160 >  /sys/devices/platform/i8042/serio1/serio2/speed
