# Fresh NixOS install on ZFS

Two scripts that install a new laptop from this flake with the same disk
layout as kanta. Run them from the NixOS live USB.

Layout on the NVMe:

| part | size | content |
|------|------|---------|
| 1    | 1G   | EFI, vfat, mounted at /boot |
| 2    | 64G  | LUKS2, swap inside, used for hibernation |
| 3    | rest | ZFS pool `laptop`, native encryption |

Pool and datasets mirror kanta (ashift 13, aes-256-gcm passphrase prompt,
compression on, mountpoint none, zfsutil mounts) plus atime=off and
dnodesize=auto. Datasets: laptop/nixos/nix, laptop/reserved (1G
refreservation), laptop/system/root, laptop/system/var (posix ACLs),
laptop/user/home.

Swap is a separate LUKS partition because ZFS cannot hold a hibernation
image. The same passphrase unlocks both, but it is typed twice at every boot
(cryptsetup and zfs load-key cannot share it).

## Before you start

1. Boot the target from the NixOS live USB, get its IP, make sure your ssh
   key is in its authorized_keys.
2. Add the host to the flake if it is new:
   - `nix/nixos/<host>/default.nix`: hostName, hostId, interfaces, swap and
     hibernation settings (copy from kanta2). Generate the hostId with
     `head -c 4 /dev/urandom | od -A none -t x4`.
   - `nix/nixos/<host>/hardware-configuration.nix`: take the kernel modules
     from `nixos-generate-config --show-hardware-config --no-filesystems`
     on the live USB, the fileSystems from kanta2.
   - `nixosConfigurations.<host>` and `homeConfigurations."deni@<host>"` in
     `flake.nix`.
   Leave `@BOOT_UUID@` and `@LUKS_UUID@` as placeholders, commit.
3. Copy the checkout to the live USB (it has no key for GitHub):

       rsync -a --delete ~/dotfiles/ nixos@<ip>:~/dotfiles/

## Step 1: disk

    ssh -t nixos@<ip> sudo ~/dotfiles/nix/bootstrap/01-disk.sh

Asks for YES and for the passphrase, then partitions, creates LUKS swap and
the pool, mounts everything under /mnt and prints a report. Environment
overrides: `DISK` (default /dev/nvme0n1), `SWAP_SIZE` (default 64G, must be
at least the RAM size for hibernation).

Take `BOOT_UUID` and `LUKS_UUID` from the report, put them into
`nix/nixos/<host>/`, commit, rsync the checkout to the live USB again.

## Step 2: install

    ssh -t nixos@<ip> sudo ~/dotfiles/nix/bootstrap/02-install.sh <host>

Copies ~/dotfiles to /mnt/home/deni/dotfiles, runs
`nixos-install --flake`, asks for the root password and then for deni's
password, and finally exports the pool. The clean export matters: the host
has forceImportRoot off (required for hibernation), so a pool that was not
exported refuses to import on first boot.

Remove the USB stick and reboot. At boot: passphrase for cryptswap, then
passphrase for the pool.

## After first boot

- `cd ~/dotfiles/nix && make system && make user`. The Makefile picks the
  flake attribute from the hostname.
- If a throwaway passphrase was used during install, change both:

      sudo cryptsetup luksChangeKey /dev/disk/by-uuid/<LUKS_UUID>
      sudo zfs change-key laptop

- Restore home from the zrepl backup on melisandre (raw encrypted send, needs
  the old machine's passphrase once):

      ssh melisandre sudo zfs send -w rust-pool-1/data/backups/zrepl/remote_sink/kanta/laptop/user/home@<snap> \
        | sudo zfs receive -u laptop/user/home_restore
      sudo zfs load-key laptop/user/home_restore
      sudo zfs change-key -i laptop/user/home_restore
      sudo zfs rename laptop/user/home laptop/user/home_fresh
      sudo zfs rename laptop/user/home_restore laptop/user/home

  Then copy ~/dotfiles from home_fresh, reboot, destroy home_fresh.
- Add the new host as a client on the zrepl receiver, the job name is
  `<host>_home_backup`.
- Optional: enroll the swap in the TPM with systemd-cryptenroll to get down
  to one passphrase prompt. Weaker than the passphrase for the hibernation
  image, which holds the ZFS keys in RAM.

## Do not

- Never import the pool from a live USB while the machine is hibernated. The
  hibernated kernel still holds it open and a forced import corrupts it.
- Never run 01-disk.sh on a machine you care about. It defaults to
  /dev/nvme0n1 and wipes it after a single YES.
