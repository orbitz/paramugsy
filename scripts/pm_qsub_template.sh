#!/bin/bash -ex
cd /opt
export MUGSY_INSTALL=$PWD/mugsy_x86-64
export PATH=$PATH:$MUGSY_INSTALL:$MUGSY_INSTALL/mapping
export PERL5LIB=$MUGSY_INSTALL/perllibs
export PATH=$PATH:$MUGSY_INSTALL/MUMmer3.20
export PATH=$PATH:/opt/paramugsy
mkdir -p /mnt/tmp
cd /mnt/tmp


