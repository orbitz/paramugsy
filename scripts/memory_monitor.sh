#!/bin/bash -e

case "$1" in
    nucmer)
	ps auxww | grep -E '(nucmer|mummer)' | grep -vE '(grep|memory_monitor)' | awk '{ SUM += $6} END { print SUM }'
	;;
    mugsy)
	ps auxww | grep mugsy | grep -vE '(nucmer|memory_monitor)' | awk '{ SUM += $6} END { print SUM }'
	;;
    all)
	ps auxww | grep -E '(mugsy|nucmer|mummer)' | grep -v 'memory_monitor' | awk '{ SUM += $6} END { print SUM }'
	;;
esac
