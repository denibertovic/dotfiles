#!/bin/bash

# This will list what process has inotify wathces active

ps -p $(find /proc/*/fd/* -type l -lname 'anon_inode:inotify' -print 2> /dev/null | sed -e 's/^\/proc\/\([0-9]*\)\/.*/\1/')
