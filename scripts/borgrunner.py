#!/usr/bin/env python

from datetime import datetime
import subprocess
import os
import sys
import logging
import argparse
import getpass

log = logging.getLogger("backup.py")
logging.basicConfig(level=logging.DEBUG)


def check_if_root():
    if getpass.getuser() != 'root':
        log.debug('Please run as root!')
        sys.exit(1)


def read_borg_pw(path):
    with open(path) as f:
        pw = f.read()
    return pw.strip()


def set_borg_env(path):
    pw = read_borg_pw(path)
    os.environ['BORG_PASSPHRASE'] = pw


def mount_backups(path, mount_script_path):
    o = run_process('mount')
    if path in o:
        log.debug('Backups volume already mounted. Doing nothing!')
        return
    run_process(sudo(mount_script_path))


def unmount_backups(path):
    run_process(sudo('umount ' + path))


def notify(msg):
    subprocess.check_output("/usr/bin/notify-send -u normal '{0}'".format(msg),
            shell=True)


def run_backup():
    log.debug("Running Borg...")
    o = run_process(sudo('/home/deni/dotfiles/scripts/borg_backup.sh'))
    log.debug("Borg output: ")
    log.debug(o)


def sudo(cmd):
    # IMPORTANT: We have to -E to preserve the env otherwise the process will prompt
    # for the password
    return "sudo -E {0}".format(cmd)


def run_process(cmd):
    try:
        out = subprocess.check_output(cmd, shell=True)
        return out
    except Exception as e:
        log.debug("Borg backup failed: {0}".format(e))
        notify("Borg backup failed. See journalctl for more details.")
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(description='Borg backup helper script')
    parser.add_argument('-w',
            '--wifi-name',
            required=True,
            metavar="WIFI_NAME",
            help='Name of the WiFi on which to start backups')
    parser.add_argument('-b',
            '--backup-path',
            required=True,
            metavar="BACKUP_PATH",
            help='Local mount path to the NFS drive')
    parser.add_argument('-m',
            '--mount-script-path',
            required=True,
            metavar="MOUNT_SCRIPT_PATH",
            help='Path to local shell script that mounts the backup volume.')
    parser.add_argument('-n',
            '--backup-name',
            required=True,
            metavar="BACKUP_NAME",
            help='Name of the Borg repository. Will get appended to BACKUP_PATH')
    parser.add_argument('-p',
            '--password-file-path',
            required=True,
            metavar="PASSWORD_FILE_PATH",
            help='Path to the password file for the borg repository')
    args = parser.parse_args()
    # Need to set this for the message to show up
    # TODO: Figure out if this works on multiple monitors
    today = datetime.today().strftime("%Y-%m-%d")
    os.environ['REPOSITORY'] = args.backup_path + "/" + args.backup_name
    os.environ['DISPLAY'] = ":0"
    log.debug("Running backup script")
    out = run_process("nmcli -t -f active,ssid dev wifi")
    if ('yes:' + args.wifi_name) in out:
        log.debug("Connected to home wifi. Starting backup process.")
        log.debug("Mounting backup volume.")
        mount_backups(args.backup_path, args.mount_script_path)
        set_borg_env(args.password_file_path)
        out = run_process(sudo('borg list ' + args.backup_path + '/' + args.backup_name))
        if today in out:
            log.debug("Today's backup is already there. Doing nothing.")
        else:
            log.debug("Backing up stuffz!")
            log.debug("Notifying user.")
            notify("Borgbackup started.")
            run_backup()
            notify("Borgbackup finished.")

        log.debug("Unmounting backup volume.")
        unmount_backups(args.backup_path)

if __name__ == "__main__":
    main()

