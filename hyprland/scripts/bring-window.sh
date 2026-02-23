#!/bin/bash
# List all windows and bring selected to current workspace
WINDOWS=$(hyprctl clients -j | jq -r '.[] | select(.workspace.name | startswith("special") | not) | "\(.address)|\(.workspace.id)|\(.class) - \(.title)"')

if [[ -z "$WINDOWS" ]]; then
  notify-send "No windows"
  exit 0
fi

# Format for display: "WS# | Class - Title"
DISPLAY=$(echo "$WINDOWS" | awk -F'|' '{print "["$2"] "$3}')

SELECTED=$(echo "$DISPLAY" | rofi -dmenu -p "Bring window")

if [[ -n "$SELECTED" ]]; then
  # Extract workspace number from selection
  WS_NUM=$(echo "$SELECTED" | sed 's/\[\([0-9]*\)\].*/\1/')
  TITLE=$(echo "$SELECTED" | sed 's/\[[0-9]*\] //')

  ADDR=$(echo "$WINDOWS" | grep "$TITLE" | cut -d'|' -f1)
  CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')

  hyprctl dispatch movetoworkspace "$CURRENT_WS,address:$ADDR"
  hyprctl dispatch focuswindow "address:$ADDR"
fi
