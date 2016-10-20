#!/bin/bash

if [ $# -eq 0 ]
  then
    echo "No arguments supplied."
    echo "Usage: ./get_essentials.sh 192.168.0.123"
    exit 1;
fi

from=$1

rsync -avz --progress $from:projects ~
rsync -avz --progress $from:work ~
rsync -avz --progress $from:.mozilla ~
rsync -avz --progress $from:.icedove ~
rsync -avz --progress $from:.weechat ~

