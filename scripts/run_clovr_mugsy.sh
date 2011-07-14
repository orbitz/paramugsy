#!/bin/bash

cd /mnt/mugsy-devel

source env.sh

cd clovr-mugsy

echo "Running:" $@
$@
