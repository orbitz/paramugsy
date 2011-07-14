#!/bin/bash -e

QUEUE=$1
PRIORITY=$2
SRC_PATH=$3
DST_PATH=$4

mkdir -p `dirname $DST_PATH`

qsub -p $PRIORITY -o /mnt/scratch -e /mnt/scratch -S /bin/sh -b y -sync y -q $QUEUE /opt/paramugsy/rsync_to.sh `hostname -f` $SRC_PATH $DST_PATH
