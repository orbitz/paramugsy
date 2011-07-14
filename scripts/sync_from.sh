#!/bin/bash -e

QUEUE=$1
PRIORITY=$2
SRC_PATH=$3
DST_PATH=$4

qsub -p $PRIORITY -o /mnt/scratch -e /mnt/scratch -S /bin/sh -b y -sync y -q $QUEUE /opt/paramugsy/rsync_from.sh `hostname -f` $SRC_PATH $DST_PATH
