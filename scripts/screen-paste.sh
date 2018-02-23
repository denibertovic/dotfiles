#!/bin/bash
#
# screen-paste - Paste a screenshot to the Web (using pixbin.us service)
#
# Written by Senko Rasic <senko.rasic@goodcode.io>
# Released into Public Domain. Use it as you like.
#
# The tool allows the user to select a portion of the screen, then copies it
# to pixbin.us, and stores the resulting URL in clipboard.
#
# Requirements: uuidgen, scrot, curl, xclip, mplayer (optional)

set -ex

FDO_SOUNDS_FOLDER="/usr/share/sounds/freedesktop/stereo/"
ATTN_SOUND="window-attention.oga"
SHUTTER_SOUND="camera-shutter.oga"

function play_sound() {
    SOUND="$FDO_SOUNDS_FOLDER/$1"
    if [ ! -z "$(which mplayer)" ] && test -f "$SOUND"; then
        mplayer -really-quiet "$SOUND" >/dev/null 2>&1
    fi
}

fname="/tmp/$(uuidgen).png"
fname_b64="/tmp/$(uuidgen).b64"

test -f "$fname" && rm -f "$fname"
play_sound "$ATTN_SOUND"
scrot -z -s "$fname"
base64 < "$fname" > "$fname_b64"

curl -F "caption=ScreenPaste" -F "image=<$fname_b64" \
        http://pixbin.us/api/post/ | \
    cut -f4 -d'"' | \
    xclip -i -selection "clipboard"

rm -f "$fname" "$fname_b64"
play_sound "$SHUTTER_SOUND"
