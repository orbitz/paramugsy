#!/bin/bash -ex

HOSTNAME=$1
SRC_PATH=$2
DST_PATH=$3

mkdir -p `dirname $DST_PATH`

rsync -av -e "ssh -oNoneSwitch=yes -oNoneEnabled=yes -o PasswordAuthentication=no -o ConnectTimeout=30 -o StrictHostKeyChecking=no -o ServerAliveInterval=30 -o UserKnownHostsFile=/dev/null -q -i /mnt/keys/devel1.pem" root@$HOSTNAME:$SRC_PATH $DST_PATH
