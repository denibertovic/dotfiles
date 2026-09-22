#!/usr/bin/env bash
# Step 2 of a fresh install: install NixOS into /mnt from the dotfiles flake,
# set passwords, then cleanly export the pool so the first boot can import it
# without force (required, forceImportRoot is off for hibernation).
# Expects ~/dotfiles on the live USB (rsync it from another machine first,
# the live USB has no key for GitHub).
# Run on the live USB as: sudo ./02-install.sh <host>
set -euo pipefail

POOL=laptop
HOST="${1:-}"
SRC="${DOTFILES_SRC:-/home/nixos/dotfiles}"
DEST=/mnt/home/deni/dotfiles

[ "$(id -u)" = 0 ] || { echo "run with sudo"; exit 1; }
[ -n "$HOST" ] || { echo "usage: sudo ./02-install.sh <host>   (flake attribute, e.g. kanta2)"; exit 1; }
findmnt /mnt >/dev/null || { echo "/mnt not mounted, run 01-disk.sh first"; exit 1; }
findmnt /mnt/boot >/dev/null || { echo "/mnt/boot not mounted"; exit 1; }
[ -f "$SRC/nix/flake.nix" ] || { echo "no flake at $SRC/nix/flake.nix"; exit 1; }
[ -d "$SRC/nix/nixos/$HOST" ] || { echo "no host dir $SRC/nix/nixos/$HOST"; exit 1; }
if grep -rq '@[A-Z_]*UUID@' "$SRC/nix/nixos/$HOST"; then
  echo "placeholders still present in $SRC/nix/nixos/$HOST:"; grep -rn '@[A-Z_]*UUID@' "$SRC/nix/nixos/$HOST"; exit 1
fi

# Put the flake where it will live on the installed system, so the first
# nixos-rebuild after boot works from the same checkout.
mkdir -p "$(dirname "$DEST")"
rsync -a --delete "$SRC/" "$DEST/"
chown -R 1000:100 /mnt/home/deni

echo "Installing $HOST from $DEST/nix. nixos-install asks for the root password at the end."
nixos-install --root /mnt --flake "$DEST/nix#$HOST"

echo "Set the password for user deni:"
nixos-enter --root /mnt -c 'passwd deni'

echo
echo "===== REPORT ====="
ls /mnt/boot/EFI /mnt/boot/loader/entries 2>/dev/null
ls -la /mnt/nix/var/nix/profiles/ 2>/dev/null
echo "===== END REPORT ====="

read -r -p "Unmount, export pool '$POOL' and get ready to reboot? [y/N] " ans
if [ "$ans" = "y" ] || [ "$ans" = "Y" ]; then
  umount -R /mnt
  zpool export "$POOL"
  cryptsetup close cryptswap
  echo "Done. Remove the USB stick and run: sudo reboot"
  echo "At boot you type the passphrase twice: once for cryptswap, once for the ZFS pool."
else
  echo "Left mounted. Before reboot run: umount -R /mnt && zpool export $POOL && cryptsetup close cryptswap"
fi
