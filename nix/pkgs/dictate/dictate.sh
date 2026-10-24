# Push-to-talk dictation that stays on this machine. The first run starts
# recording, the next one stops it, transcribes it with whisper.cpp and
# puts the text in the clipboard. Nothing is typed into a window, so the
# text can be checked before it is sent to an agent.

usage() {
  cat >&2 <<USAGE
Usage: dictate [command]

  toggle   start recording, or stop and transcribe (default)
  cancel   stop recording and throw the audio away
USAGE
  exit 2
}

dir="${XDG_RUNTIME_DIR:-/tmp}/dictate"
rec_pid="$dir/rec.pid"
tick_pid="$dir/tick.pid"
nid_file="$dir/nid"
busy="$dir/busy"
wav="$dir/in.wav"

# Whisper uses this as earlier context, which makes it spell jargon the
# way it appears here.
prompt="${DICTATE_PROMPT:-NixOS, flake, home-manager, xmonad, tmux, rofi, PipeWire, Claude Code, Haskell, git, GitHub, PR}"

# xmobar shows this root window property through XPropertyLog, so the
# bar changes at once and nothing polls.
bar() { xprop -root -f _DICTATE 8u -set _DICTATE "$1"; }

# Each call replaces the last notification, so dunst shows one entry that
# follows the state instead of a stack of them.
notify() {
  local timeout=$1 body=$2 nid
  nid=$(cat "$nid_file" 2>/dev/null || true)
  notify-send -a dictate -t "$timeout" -p -r "${nid:-0}" "Dictate" "$body" >"$nid_file"
}

recording() { [ -f "$rec_pid" ] && kill -0 "$(cat "$rec_pid")" 2>/dev/null; }

wait_gone() { while kill -0 "$1" 2>/dev/null; do sleep 0.05; done; }

tick() {
  local start=$1 pid=$2 t elapsed
  while kill -0 "$pid" 2>/dev/null; do
    t=$(($(date +%s) - start))
    elapsed=$(printf '%d:%02d' $((t / 60)) $((t % 60)))
    bar "<fc=#ff5555>REC $elapsed</fc> "
    notify 0 "Recording $elapsed"
    sleep 1
  done
}

start() {
  mkdir -p "$dir"
  rm -f "$wav" "$nid_file"
  pw-record --rate 16000 --channels 1 "$wav" &
  echo $! >"$rec_pid"
  tick "$(date +%s)" "$!" &
  echo $! >"$tick_pid"
}

end_recording() {
  local pid
  # The ticker goes first, or it can write the bar again after we clear it.
  pid=$(cat "$tick_pid")
  kill "$pid" 2>/dev/null || true
  wait_gone "$pid"
  pid=$(cat "$rec_pid")
  # pw-record writes the WAV header on exit, so wait for it to finish.
  kill -INT "$pid" 2>/dev/null || true
  wait_gone "$pid"
  rm -f "$rec_pid" "$tick_pid"
}

stop() {
  touch "$busy"
  trap 'rm -f "$busy"; bar ""' EXIT
  end_recording
  bar "<fc=#ffcc00>transcribing</fc> "
  notify 0 "Transcribing"

  local text
  if ! text=$(whisper-cli -m "$DICTATE_MODEL" -f "$wav" -l en -nt -np --prompt "$prompt" 2>/dev/null |
    tr -s ' \n' ' ' | sed 's/^ *//; s/ *$//'); then
    notify 5000 "Transcription failed"
    exit 1
  fi
  if [ -z "$text" ]; then
    notify 3000 "No speech found"
    exit 1
  fi
  printf '%s' "$text" | xclip -selection clipboard
  notify 8000 "In clipboard: $text"
}

cancel() {
  end_recording
  bar ""
  notify 2000 "Cancelled"
}

case "${1:-toggle}" in
toggle)
  # A press while whisper runs would start a new recording on top of it.
  [ -f "$busy" ] && exit 0
  if recording; then stop; else start; fi
  ;;
cancel)
  if recording; then cancel; fi
  ;;
*) usage ;;
esac
