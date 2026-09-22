#!/usr/bin/env bash
# Step 1 of a fresh install: partition the NVMe, create the LUKS swap and the
# encrypted ZFS pool (same layout and properties as kanta), mount everything
# under /mnt and print the UUIDs that go into nix/nixos/<host>/.
# Run on the live USB as: sudo ./01-disk.sh
set -euo pipefail

DISK="${DISK:-/dev/nvme0n1}"
POOL=laptop
SWAP_SIZE="${SWAP_SIZE:-64G}"

[ "$(id -u)" = 0 ] || { echo "run with sudo"; exit 1; }
[ -b "$DISK" ] || { echo "$DISK not found"; exit 1; }

BYID=""
for l in /dev/disk/by-id/nvme-*; do
  case "$l" in *nvme-eui.*|*-part*) continue;; esac
  [ "$(readlink -f "$l")" = "$DISK" ] && { BYID="$l"; break; }
done
[ -n "$BYID" ] || { echo "no /dev/disk/by-id entry for $DISK"; exit 1; }

echo "This will DESTROY everything on $DISK ($BYID):"
lsblk "$DISK"
echo
echo "Layout: 1G EFI | ${SWAP_SIZE} LUKS swap | rest ZFS pool '$POOL' (encrypted)"
echo
read -r -p "Type YES to continue: " ans
[ "$ans" = "YES" ] || { echo "aborted"; exit 1; }

# One passphrase for both LUKS swap and the ZFS pool.
while :; do
  read -r -s -p "Passphrase for LUKS swap and ZFS pool: " PASS; echo
  read -r -s -p "Repeat: " PASS2; echo
  [ "$PASS" = "$PASS2" ] && [ ${#PASS} -ge 8 ] && break
  echo "mismatch or shorter than 8 chars, try again"
done
unset PASS2

# Partition
umount -R /mnt 2>/dev/null || true
zpool export -a 2>/dev/null || true
cryptsetup close cryptswap 2>/dev/null || true
wipefs -af "${DISK}"p* 2>/dev/null || true
sgdisk --zap-all "$DISK"
sgdisk -n1:0:+1G           -t1:ef00 -c1:EFI       "$DISK"
sgdisk -n2:0:+"$SWAP_SIZE" -t2:8309 -c2:cryptswap "$DISK"
sgdisk -n3:0:0             -t3:bf01 -c3:zfs       "$DISK"
partprobe "$DISK"
udevadm settle
sleep 2
sgdisk -p "$DISK"

P1="${DISK}p1"; P2="${DISK}p2"; P3="${BYID}-part3"
[ -b "$P3" ] || { echo "$P3 missing after partitioning"; exit 1; }

# EFI
mkfs.vfat -F32 -n EFI "$P1"

# LUKS swap
printf '%s' "$PASS" | cryptsetup luksFormat --type luks2 -q "$P2" -
printf '%s' "$PASS" | cryptsetup open --key-file=- "$P2" cryptswap
mkswap -L swap /dev/mapper/cryptswap

# ZFS pool, same properties as kanta plus atime=off and dnodesize=auto
# (dnodesize=auto pairs with xattr=sa; fine with systemd-boot, not with GRUB)
printf '%s' "$PASS" | zpool create -f \
  -o ashift=13 \
  -O encryption=aes-256-gcm -O keyformat=passphrase -O keylocation=prompt \
  -O mountpoint=none -O compression=on -O atime=off -O dnodesize=auto \
  -R /mnt "$POOL" "$P3"
unset PASS

zfs create -o mountpoint=none "$POOL/nixos"
zfs create "$POOL/nixos/nix"
zfs create -o mountpoint=none -o refreservation=1G "$POOL/reserved"
zfs create -o mountpoint=none "$POOL/system"
zfs create -o mountpoint=none "$POOL/system/root"
zfs create -o xattr=sa -o acltype=posix "$POOL/system/var"
zfs create -o mountpoint=none -o com.sun:auto-snapshot=true "$POOL/user"
zfs create "$POOL/user/home"

# Mount
mount -t zfs -o zfsutil "$POOL/system/root" /mnt
mkdir -p /mnt/nix /mnt/var /mnt/home /mnt/boot
mount -t zfs -o zfsutil "$POOL/nixos/nix"   /mnt/nix
mount -t zfs -o zfsutil "$POOL/system/var"  /mnt/var
mount -t zfs -o zfsutil "$POOL/user/home"   /mnt/home
mount "$P1" /mnt/boot

echo
echo "===== REPORT ====="
lsblk -o NAME,SIZE,TYPE,FSTYPE,MOUNTPOINTS "$DISK"
zpool status "$POOL"
zpool get ashift "$POOL"
zfs get -s local -d 2 all "$POOL" | grep -v shutdown-time
zfs get -o name,value encryption,keyformat,keylocation "$POOL"
findmnt -R /mnt
echo "BOOT_UUID=$(blkid -s UUID -o value "$P1")"
echo "LUKS_UUID=$(blkid -s UUID -o value "$P2")"
echo "--- nixos-generate-config --show-hardware-config (for comparison only)"
nixos-generate-config --root /mnt --show-hardware-config
echo "===== END REPORT ====="
echo "Next: put BOOT_UUID and LUKS_UUID into nix/nixos/<host>/, rsync dotfiles here, then: sudo ./02-install.sh"
