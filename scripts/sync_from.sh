#!/bin/bash -e

QUEUE=$1
PRIORITY=$2
FILE_LIST=$3
SRC_PATH=$4
DST_PATH=$5

qsub -p $PRIORITY -o /mnt/scratch -e /mnt/scratch -S /bin/sh -b y -sync y -q $QUEUE /opt/paramugsy/rsync_from.sh `hostname -f` $FILE_LIST $SRC_PATH $DST_PATH
