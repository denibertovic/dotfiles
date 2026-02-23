#!/bin/bash
# List hidden windows and let user select one to bring back
HIDDEN=$(hyprctl clients -j | jq -r '.[] | select(.workspace.name == "special:hidden") | "\(.address)|\(.class) - \(.title)"')

if [[ -z "$HIDDEN" ]]; then
  notify-send "No hidden windows"
  exit 0
fi

# Use rofi in dmenu mode
SELECTED=$(echo "$HIDDEN" | cut -d'|' -f2 | rofi -dmenu -p "Unhide window")

if [[ -n "$SELECTED" ]]; then
  ADDR=$(echo "$HIDDEN" | grep "$SELECTED" | cut -d'|' -f1)
  CURRENT_WS=$(hyprctl activeworkspace -j | jq -r '.id')
  hyprctl dispatch movetoworkspace "$CURRENT_WS,address:$ADDR"
  hyprctl dispatch focuswindow "address:$ADDR"
fi
