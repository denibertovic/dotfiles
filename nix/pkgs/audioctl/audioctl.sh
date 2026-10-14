# Switch the default PipeWire output or input by name. WirePlumber moves
# the running streams to the new default and remembers the choice.

usage() {
  cat >&2 <<USAGE
Usage: audioctl <command>

  sink     pick the default output with rofi
  mic      pick the default input with rofi
  next     switch the output to the next one in the list
  status   show the current default output and input
USAGE
  exit 2
}

notify() { notify-send -a audioctl "Audio" "$1"; }

die() {
  notify "$1"
  echo "audioctl: $1" >&2
  exit 1
}

# Node ids change on every reconnect, so the dump is read fresh each run
# and nothing is stored between runs. A node whose jack or port is not
# plugged (an HDMI output with no display, the stereo mic) is left out:
# WirePlumber refuses to make it the default anyway. Rows: id, node.name,
# description.
nodes() {
  local class=$1 dir=Output
  [ "$class" = Audio/Source ] && dir=Input
  jq -r --arg class "$class" --arg dir "$dir" '
    [ .[] | select(.type == "PipeWire:Interface:Device") | .id as $dev
      | (.info.params.EnumRoute // [])[]
      | select(.direction == $dir and .available == "no")
      | .devices[] | "\($dev)/\(.)" ] as $unplugged
    | [ .[] | select(.info.props["media.class"] == $class)
        | select("\(.info.props["device.id"])/\(.info.props["card.profile.device"])" | IN($unplugged[]) | not) ]
    | sort_by(.info.props["node.description"])
    | .[] | [.id, .info.props["node.name"], .info.props["node.description"]] | @tsv
  ' <<<"$dump"
}

default_name() {
  jq -r --arg key "default.audio.$1" '
    .[] | select(.type == "PipeWire:Interface:Metadata" and .props["metadata.name"] == "default")
    | .metadata[] | select(.key == $key) | .value.name
  ' <<<"$dump"
}

set_default() {
  local id=$1 desc=$2
  if wpctl set-default "$id"; then
    notify "$desc"
  else
    die "cannot switch to $desc"
  fi
}

# $1 is sink or source, $2 the rofi prompt. Rofi shows only the description
# column but returns the whole row, so the id comes back with the choice.
pick() {
  local kind=$1 prompt=$2 class current rows choice id desc
  class=Audio/Sink
  [ "$kind" = source ] && class=Audio/Source
  current=$(default_name "$kind")
  rows=$(nodes "$class" | awk -F'\t' -v cur="$current" -v OFS='\t' '
    { mark = ($2 == cur) ? "* " : "  "; print $1, mark $3 }')
  [ -n "$rows" ] || die "no $kind found"
  choice=$(rofi -dmenu -i -p "$prompt" -display-columns 2 -display-column-separator '\t' <<<"$rows") || exit 0
  id=${choice%%$'\t'*}
  desc=${choice#*$'\t'}
  set_default "$id" "${desc#[* ] }"
}

dump=$(pw-dump) || die "pw-dump failed, is PipeWire running?"

case "${1:-}" in
  sink)
    pick sink "Output"
    ;;
  mic)
    pick source "Input"
    ;;
  next)
    current=$(default_name sink)
    # awk must read the whole list: an early exit breaks the pipe from jq
    # and pipefail then makes the assignment fail
    row=$(nodes Audio/Sink | awk -F'\t' -v cur="$current" '
      { rows[NR] = $0; if ($2 == cur) i = NR }
      END { if (NR) print rows[i % NR + 1] }')
    [ -n "$row" ] || die "no output found"
    IFS=$'\t' read -r id _ desc <<<"$row"
    set_default "$id" "$desc"
    ;;
  status)
    for kind in sink source; do
      current=$(default_name "$kind")
      printf '%-7s %s\n' "$kind:" "$(nodes "Audio/$([ "$kind" = sink ] && echo Sink || echo Source)" |
        awk -F'\t' -v cur="$current" '$2 == cur { print $3 }')"
    done
    ;;
  *)
    usage
    ;;
esac
