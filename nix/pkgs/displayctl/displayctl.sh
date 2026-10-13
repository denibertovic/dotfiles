# Switch between display setups by name. Profiles live in autorandr
# (programs.autorandr in home-manager); this is the human-facing front end.

usage() {
  cat >&2 <<USAGE
Usage: displayctl <mode>

  laptop   laptop panel only
  work     portrait Dell 23 | Dell 27 (primary) | laptop panel
  mirror   clone the laptop panel onto whatever external display is attached
  extend   put the external display to the right of the laptop panel
  auto     work if the Dells are attached, mirror for any other external display
  menu     pick a mode with rofi
  status   show the profile that fits the attached displays, and the layout
USAGE
  exit 2
}

# The dock exposes the Dells under whichever DP-x-y names the MST hub gets,
# so always match profiles by EDID instead of output name.
ar() { autorandr --match-edid "$@"; }

notify() { notify-send -a displayctl "Display" "$1"; }

die() {
  notify "$1"
  echo "displayctl: $1" >&2
  exit 1
}

external_connected() {
  xrandr --query | grep -q -E '^(DP|HDMI)[^ ]* connected'
}

load() {
  local out
  if out=$(ar --load "$1" 2>&1); then
    notify "$1"
  else
    die "$1 failed: $out"
  fi
}

mode=${1:-}
case "$mode" in
  laptop | work)
    load "$mode"
    ;;
  mirror)
    external_connected || die "no external display connected"
    # common needs a mode both displays support; a projector almost always
    # has one. Scaling (clone-largest) is the fallback for the odd case.
    if ar --load common 2>/dev/null; then
      notify "mirror"
    else
      load clone-largest
    fi
    ;;
  extend)
    external_connected || die "no external display connected"
    load horizontal
    ;;
  auto)
    detected=$(ar --detected 2>/dev/null | sed -n 1p)
    if [ -n "$detected" ]; then
      load "$detected"
    elif external_connected; then
      exec "$0" mirror
    else
      load laptop
    fi
    ;;
  menu)
    choice=$(printf '%s\n' laptop work mirror extend auto | rofi -dmenu -i -p "Display")
    [ -n "$choice" ] || exit 0
    exec "$0" "$choice"
    ;;
  status)
    # autorandr --current needs every driver x-prop to match the profile,
    # so only report which profile fits the attached displays
    echo "detected profile: $(ar --detected 2>/dev/null | sed -n 1p)"
    xrandr --listmonitors
    ;;
  *)
    usage
    ;;
esac
