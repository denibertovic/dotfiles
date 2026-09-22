#!/usr/bin/env bash
# Step 3: move the home dataset from the OLD laptop to a freshly installed
# one, as a raw (still encrypted) ZFS send over ssh, and swap it in place.
# Run on the OLD laptop as your user: ./03-restore-home.sh deni@<new-ip>
#
# Sends the latest zrepl_ snapshot first (it also exists on the backup
# server, so the new machine can later continue incrementally against the
# renamed backup tree) and then an incremental to a fresh snapshot taken
# now. The transfer is resumable: rerun the script after an interruption.
set -euo pipefail

TARGET="${1:?usage: $0 deni@<new-host-ip>}"
SRC=laptop/user/home
DST=laptop/user/home_restore
FRESH=laptop/user/home_orig
R="ssh -o BatchMode=yes $TARGET"

# Sanity
$R hostname >/dev/null || { echo "cannot ssh to $TARGET"; exit 1; }
[ "$($R hostname)" != "$(hostname)" ] || { echo "target is this machine"; exit 1; }
$R zfs list -H -o name laptop/user/home >/dev/null || { echo "no laptop/user/home on target"; exit 1; }
if $R zfs list -H -o name "$FRESH" >/dev/null 2>&1; then
  echo "$FRESH already exists on target: the swap already happened"; exit 1
fi
if ip route get "${TARGET#*@}" | grep -q " dev wl"; then
  echo "WARNING: route to target goes over wifi. 200G+ takes hours that way. Plug in a cable (ctrl-c now if you want)."
  sleep 5
fi

# kanta must stop taking and pushing snapshots while we migrate.
if systemctl is-active --quiet zrepl; then
  echo "stopping zrepl on $(hostname)"
  sudo systemctl stop zrepl
fi

BASE="$(zfs list -t snapshot -o name -s creation -H "$SRC" | grep '@zrepl_' | tail -1)"
[ -n "$BASE" ] || { echo "no zrepl_ snapshot on $SRC"; exit 1; }
MIG="$(zfs list -t snapshot -o name -s creation -H "$SRC" | grep '@migrate-' | tail -1 || true)"
if [ -z "$MIG" ]; then
  MIG="$SRC@migrate-$(date +%Y%m%d-%H%M)"
  sudo zfs snapshot "$MIG"
fi
echo "base snapshot:    $BASE"
echo "final snapshot:   $MIG"
echo "to send now:      $(zfs list -H -o refer "$BASE") + delta"

# Phase 1: full send of the base snapshot, resumable.
TOKEN="$($R sudo zfs get -H -o value receive_resume_token "$DST" 2>/dev/null || echo "-")"
if [ "$TOKEN" != "-" ] && [ -n "$TOKEN" ]; then
  echo "resuming interrupted transfer"
  sudo zfs send -v -t "$TOKEN" | $R sudo zfs receive -s -u "$DST"
elif ! $R zfs list -H -o name "$DST" >/dev/null 2>&1; then
  echo "full send of $BASE"
  sudo zfs send -w -v "$BASE" | $R sudo zfs receive -s -u "$DST"
else
  echo "$DST already on target"
fi

# Phase 2: incremental base -> migrate.
if ! $R zfs list -H -o name "$DST@${MIG#*@}" >/dev/null 2>&1; then
  TOKEN="$($R sudo zfs get -H -o value receive_resume_token "$DST" 2>/dev/null || echo "-")"
  if [ "$TOKEN" != "-" ] && [ -n "$TOKEN" ]; then
    sudo zfs send -v -t "$TOKEN" | $R sudo zfs receive -s -u "$DST"
  else
    echo "incremental send $BASE -> $MIG"
    sudo zfs send -w -v -i "$BASE" "$MIG" | $R sudo zfs receive -s -u "$DST"
  fi
fi

# Phase 3: key. The raw copy carries its own key wrapped with the old
# passphrase; load it once and make it inherit the target pool key.
if [ "$($R zfs get -H -o value keystatus "$DST")" != "available" ]; then
  read -r -s -p "Pool passphrase of the OLD laptop: " PASS; echo
  printf '%s' "$PASS" | $R sudo zfs load-key "$DST"
  unset PASS
fi
if [ "$($R zfs get -H -o value encryptionroot "$DST")" != "laptop" ]; then
  $R sudo zfs change-key -i "$DST"
fi

# Phase 4: swap. Nothing may use /home on the target, so this runs as a
# transient root unit that survives the end of our ssh session and of the
# desktop session it terminates.
echo "swapping datasets on target (this logs deni out there)"
# Single quoted for the remote shell: ssh does not preserve local quoting.
SWAP="loginctl terminate-user deni || true; sleep 8;
for i in 1 2 3 4 5 6 7 8 9 10; do umount /home && break; sleep 3; done;
findmnt /home >/dev/null && exit 1;
zfs rename laptop/user/home $FRESH && zfs rename $DST laptop/user/home && mount /home"
# Transient units get no PATH, and terminating deni also kills this ssh
# session, so its exit status means nothing.
$R "sudo systemd-run --unit=home-swap --collect -p Environment=PATH=/run/current-system/sw/bin sh -c '$SWAP'" || true
# Every ssh login as deni touches /home/deni again, so give the unit time
# before polling and poll rarely.
sleep 30
for _ in $(seq 1 20); do
  if $R findmnt -n -o SOURCE /home 2>/dev/null | grep -qx "laptop/user/home" \
     && $R zfs list -H -o name "$FRESH" >/dev/null 2>&1; then
    break
  fi
  sleep 15
done

echo
echo "===== REPORT ====="
$R sudo journalctl -u home-swap --no-pager -q | tail -5
$R zfs list -o name,used,refer,mountpoint,mounted,encryptionroot,keystatus -r laptop/user
$R findmnt /home
$R ls -la /home/deni | head -15
echo "===== END REPORT ====="
echo "Log in on the new laptop. When happy: ssh $TARGET sudo zfs destroy $FRESH"
