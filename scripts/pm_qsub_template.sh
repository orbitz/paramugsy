#!/bin/bash -ex
PARAMUGSY_INSTALL=/opt/paramugsy

export PATH=$PATH:/opt/mugsy_x86-64/MUMmer3.20

# We want paramugsy infront of everything because we are putting modified binaries in here
export PATH=$PARAMUGSY_INSTALL:$PATH

# We now want to set MUGSY_INSTALL to where PARAMUGSY is so we use all of those executables
# because we have modified them
export MUGSY_INSTALL=$PARAMUGSY_INSTALL

mkdir -p /mnt/tmp
cd /mnt/tmp


