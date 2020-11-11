#!/bin/bash                                                                                                      

MOZILLA_CONFIG=~/.mozilla
FIREFOX='/usr/bin/firefox'


NAMES=($(awk -F "=" '/Name/ {print $2}' $MOZILLA_CONFIG/firefox/profiles.ini | tr -d ' '))
if [ -z $@ ]
then
    echo "Profile Manager"
    for name in "${NAMES[@]}"
    do
        echo $name
    done
else
    PATHS=($(awk -F "=" '/Path/ {print $2}' $MOZILLA_CONFIG/firefox/profiles.ini | tr -d ' '))
    PROFILE=$@
    for i in "${!NAMES[@]}"; do
        if [[ "${NAMES[$i]}" = "${PROFILE}" ]]; then
            $FIREFOX --new-instance --profile "$MOZILLA_CONFIG/firefox/${PATHS[$i]}" >/dev/null &
            exit
        fi
    done

    $FIREFOX --new-instance --ProfileManager >/dev/null &
fi