# Push-to-talk dictation that stays on this machine. The first run starts
# recording, the next one stops it, transcribes it with whisper.cpp and
# puts the text in the clipboard. Nothing is typed into a window, so the
# text can be checked before it is sent to an agent.

dir="${XDG_RUNTIME_DIR:-/tmp}/dictate"
pidfile="$dir/pid"
wav="$dir/in.wav"

# Whisper uses this as earlier context, which makes it spell jargon the
# way it appears here.
prompt="${DICTATE_PROMPT:-NixOS, flake, home-manager, xmonad, tmux, rofi, PipeWire, Claude Code, Haskell, git, GitHub, PR}"

notify() { notify-send -a dictate -t 2000 "Dictate" "$1"; }

start() {
  mkdir -p "$dir"
  rm -f "$wav"
  pw-record --rate 16000 --channels 1 "$wav" &
  echo $! >"$pidfile"
  notify "Recording"
}

stop() {
  local pid
  pid=$(cat "$pidfile")
  rm -f "$pidfile"
  kill -INT "$pid" 2>/dev/null || true
  # pw-record writes the WAV header on exit, so wait for it to finish.
  while kill -0 "$pid" 2>/dev/null; do sleep 0.05; done

  notify "Transcribing"
  local text
  text=$(whisper-cli -m "$DICTATE_MODEL" -f "$wav" -l en -nt -np --prompt "$prompt" 2>/dev/null |
    tr -s ' \n' ' ' | sed 's/^ *//; s/ *$//')
  if [ -z "$text" ]; then
    notify "No speech found"
    exit 1
  fi
  printf '%s' "$text" | xclip -selection clipboard
  notify "In clipboard: $text"
}

if [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; then
  stop
else
  start
fi
