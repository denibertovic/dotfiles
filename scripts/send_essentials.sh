#!/bin/bash

if [ $# -eq 0 ]
  then
    echo "No arguments supplied."
    echo "Usage: ./send_essentials.sh 192.168.0.123"
    exit 1;
fi

to=$1

rsync -avz --progress ~/projects $to:~
rsync -avz --progress ~/work $to:~
rsync -avz --progress ~/.mozilla $to:~
rsync -avz --progress ~/.icedove $to:~
rsync -avz --progress ~/.weechat $to:~

