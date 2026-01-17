#!/bin/bash
#@(#) rebuild pages
# requires a lot of infrastructure to run, including
# pandoc
# fpm-standalone
# fpm
# fortran compiler, defaults to gfortran
export FMAN_COLOR=
declare +x FMAN_COLOR
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
mkdir -p standalone
mv standalone.[fF]90 standalone/fman.F90
cd standalone
gfortran fman.F90 -o fpm-man
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
fpm-man -lines 0 manual |grep '##'
fpm-man -lines 0 manual |findll -l 80
echo 'Check for lines that will be accidently seen as *roff directives'
grep '^\.' txt/*.man
grep ' _' txt/*.man
file md?/*.md|grep -v ASCII
)|tee /tmp/M_intrinsics.log
######################################
fpm install
cp standalone/fman.F90 $HOME/github/lockstockandbarrel/mars/bootstrap/fman.F90
cp docs/manpages7.zip $HOME/github/lockstockandbarrel/mars/docs/7fortran.zip
cp docs/manpages5.zip $HOME/github/lockstockandbarrel/mars/docs/5fortran.zip
cp docs/manpages3.zip  $HOME/github/lockstockandbarrel/mars/docs/3fortran.zip
######################################
# man-page source for building GPF (General Purpose Fortran) library documents
cp txt/* $HOME/LIBRARY/libGPF/download/doc/
######################################
(
# build and install fpm-man
bash scripts/rebuild.sh
)
cp `which fpm-man` `which fman`
env FMAN_COLORS= fpm man manual --lines=0 >docs/manual.txt
fpm man --color --lines=0 manual |ansi2html>docs/manual.html
######################################
exit
######################################
https://fortran-lang.org/en/learn/intrinsics/array/#pack
