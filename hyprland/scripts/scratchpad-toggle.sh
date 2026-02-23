#!/bin/bash
# Usage: scratchpad-toggle.sh <name> <class-pattern> <launch-command>
# Mimics XMonad scratchpad: brings window to current WS, or hides it back
NAME="$1"
CLASS_PATTERN="$2"
LAUNCH_CMD="$3"

SPECIAL_WS="special:$NAME"
CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')

# Find window by class pattern (anywhere - current WS, special WS, or other)
WINDOW=$(hyprctl clients -j | jq -r ".[] | select(.class | test(\"$CLASS_PATTERN\")) | {addr: .address, ws: .workspace.name}" | head -1)
ADDR=$(echo "$WINDOW" | jq -r '.addr // empty')
WS=$(echo "$WINDOW" | jq -r '.ws // empty')

if [[ -z "$ADDR" ]]; then
  # Window doesn't exist - launch it
  eval "$LAUNCH_CMD" &
  exit 0
fi

if [[ "$WS" == "$SPECIAL_WS" ]]; then
  # Window is hidden in special workspace - bring to current workspace
  hyprctl dispatch movetoworkspace "$CURRENT_WS,address:$ADDR"
  hyprctl dispatch focuswindow "address:$ADDR"
else
  # Window is visible somewhere - hide it to special workspace
  hyprctl dispatch movetoworkspacesilent "$SPECIAL_WS,address:$ADDR"
fi
