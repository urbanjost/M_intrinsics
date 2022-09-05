#!/bin/bash
#@(#) rebuild pages
# requires a lot of infrastructure to run
(
exec 2>&1
######################################
scripts/scripts/totxt.sh
######################################
(
cd scripts
make
make demos
make ship
)
######################################
(
cd examples
make clean
)
######################################
scripts/scripts/slidy.sh
fpm build
fpm standalone
mv ffpm.f90 standalone/fpm-man.f90
######################################
)|tee /tmp/M_intrinsics.log
######################################
exit
######################################
