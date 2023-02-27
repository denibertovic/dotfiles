#!/usr/bin/env bash

# Please define the path to REPOSITORY before calling
# this scripts
if test -z "$REPOSITORY"; then echo "ERROR: REPOSITORY is not defined."; exit 1; fi;

if [ "$EUID" -ne 0 ]
  then echo "Please run as root."
  exit
fi

# Backup all of /home and /var/www except a few
# excluded directories
borg create --compression zlib,9 -v --stats \
    $REPOSITORY::'{hostname}-{now}' \
    /home/deni \
    /etc \
    --exclude-caches \
    --exclude-from '/home/deni/dotfiles/backup_exclude_patterns'

# Use the `prune` subcommand to maintain 7 daily, 4 weekly and 6 monthly
# archives of THIS machine. The '{hostname}-' prefix is very important to
# limit prune's operation to this machine's archives and not apply to
# other machine's archives also.
borg prune -v --list $REPOSITORY --prefix '{hostname}-' \
    --keep-daily=7 --keep-weekly=4 --keep-monthly=6

