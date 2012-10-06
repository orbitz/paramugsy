#!/bin/bash -e

QUEUE=$1
FILE_LIST=$2
SRC_PATH=$3
DST_PATH=$4

qsub -o /mnt/scratch -e /mnt/scratch -S /bin/sh -b y -sync y -q $QUEUE /opt/paramugsy/rsync_from.sh `hostname -f` $FILE_LIST $SRC_PATH $DST_PATH
