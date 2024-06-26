#!/bin/bash
#@(#) rebuild pages
# requires a lot of infrastructure to run, including
# pandoc
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
(
cd example
bash ../scripts/play.sh demo_*.f90 > ../docs/playground.html
)
######################################
# wrap up
set +x
fpm install
echo 'Check for long lines'
fpm-man manual |grep '##'
fpm-man manual |findll -l 80
echo 'Check for lines that will be accidently seen as *roff directives'
grep '^\.' txt/*.man
grep ' _' txt/*.man
file md/*.md|grep -v ASCII
)|tee /tmp/M_intrinsics.log
######################################
fpm install
cp standalone/fman.f90 $HOME/github/index/bootstrap/fman.f90
######################################
exit
######################################
https://fortran-lang.org/en/learn/intrinsics/array/#pack
