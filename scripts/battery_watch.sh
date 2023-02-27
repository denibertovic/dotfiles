#! /usr/bin/env bash


# To test out try changing the if statement to -le 100 so that it alaways
# triggers ans see what it output. Change back to -le 10 or adjust
# accodringly

export DISPLAY=":0"

battery_level=`acpi -b | grep "Discharging" | grep -P -o '[0-9]+(?=%)'`
# OVER 9000 HACK!
if [ ${battery_level:-9000} -le 10 ]
then
    notify-send -u CRITICAL "Battery low" "Battery level is ${battery_level}%!"
fi

