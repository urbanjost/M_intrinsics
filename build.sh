#!/bin/bash
#@(#) rebuild pages
# requires a lot of infrastructure to run, including
# panddoc
# fpm-standalone
# fpm
# fortran compiler, defaults to gfortran
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
######################################
(
fpm standalone
mv standalone.f90 standalone/fman.f90
cd standalone
gfortran fman.f90 -o fpm-man
)
######################################
)|tee /tmp/M_intrinsics.log
######################################
exit
######################################
