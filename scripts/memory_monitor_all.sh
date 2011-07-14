#!/bin/bash -e

FILEBASE=$1

memory_monitor.sh nucmer >> $FILEBASE.nucmer
memory_monitor.sh mugsy >> $FILEBASE.mugsy
memory_monitor.sh all >> $FILEBASE.all
