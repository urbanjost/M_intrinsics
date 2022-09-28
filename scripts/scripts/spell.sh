#!/bin/bash
###############################################################################################################
#@(#)aspell -l mimicks the standard unix spell(1) program, roughly.  (John S. Urban)
###############################################################################################################
PATH=$(dirname $0):$PATH
export LC_ALL='C'
if inpath aspell
then
   #cat "$@" | aspell -l --mode=none | sort -u # old aspell
   cat "$@" | aspell --mode=none list | sort -u |xargs -n 5|column -t
elif inpath ispell
then
   cat "$@" | ispell -l | sort -u|xargs -n 5|column -t
elif inpath spell
then
   cat "$@" | spell|sort -u|xarg -n 5|column -t
else
  echo 'Could not find spell, aspell, ispell' 1>&2
fi
###############################################################################################################
exit


