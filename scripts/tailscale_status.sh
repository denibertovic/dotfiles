#!/usr/bin/env bash

# RED='\033[0;31m'
# GREEN='\033[0;32m'
# NC='\033[0m' # No Color

DATA=$(curl -s --unix-socket /var/run/tailscale/tailscaled.sock http://local-tailscaled.sock/localapi/v0/status)

BACKENDSTATE=$(echo "$DATA" | jq -r .BackendState)

ACTIVENODE=$(echo "$DATA" | jq -r '.Peer[] | select(.ExitNode == true) | .HostName')

# if [ "$BACKENDSTATE" = "Running" -a -n "$ACTIVENODE" ]; then
if [ "$BACKENDSTATE" = "Running" ]; then
  # echo -e "${GREEN}ON${NC}"
  echo -e "<fc=#008000>ON</fc>"
else
  # echo -e "${RED}OFF${NC}"
  echo -e "<fc=#FF0000>OFF</fc>"
fi
