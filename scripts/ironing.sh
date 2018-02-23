#!/bin/bash

# secure laptop

# flush old rules
iptables -F
iptables -X

# permit icmp and localhost
iptables -A INPUT -p icmp -j ACCEPT
iptables -A INPUT -s 127.0.0.1 -j ACCEPT

# permit already established/related packages
iptables -A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT

# drop everything on input
iptables -A INPUT -j DROP
