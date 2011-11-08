#!/bin/bash -ex

HOSTNAME=$1
FILE_LIST=$2
SRC_PATH=$3
DST_PATH=$4

rsync -av --files-from=$FILE_LIST -e "ssh -oNoneSwitch=yes -oNoneEnabled=yes -o PasswordAuthentication=no -o ConnectTimeout=30 -o StrictHostKeyChecking=no -o ServerAliveInterval=30 -o UserKnownHostsFile=/dev/null -q -i /mnt/keys/devel1.pem" $SRC_PATH root@$HOSTNAME:$DST_PATH
