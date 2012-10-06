#!/bin/sh

IFS=$(printf "\n")

while read file
do
    echo '>'`basename $file`
    grep -v '>' $file | grep -Ev '^$'
done
