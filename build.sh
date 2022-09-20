#!/bin/bash
#@(#) rebuild pages
# requires a lot of infrastructure to run, including
# panddoc
# fpm-standalone
# fpm
# fortran compiler, defaults to gfortran
cd $(dirname $0)
(
exec 2>&1
set -x
######################################
scripts/scripts/totxt.sh
######################################
(
cd scripts
make
make demos
make ship
make list
)
######################################
(
cd example
make clean
)
######################################
scripts/scripts/slidy.sh
fpm build
ford ford.md
######################################
(
fpm standalone
mv standalone.f90 standalone/fman.f90
cd standalone
gfortran fman.f90 -o fpm-man
rm -f fpm-man
)
######################################
# wrap up
set +x
fpm install
echo 'Check for long lines'
fpm-man manual |findll -l 80
echo 'Check for lines that will be accidently seen as *roff directives'
grep '^\.' txt/*.man
)|tee /tmp/M_intrinsics.log
######################################
exit
######################################
