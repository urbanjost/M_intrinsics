#!/bin/sh
#set -x
# @(#)findll.sh list files with long lines. Version 1.0 04/21/92, John S. Urban
#===============================================================================
export NAME LEN
LEN=$1
if [ "$LEN" = '' ]
then
   LEN=72
else
   shift
fi
for NAME in ${*:-' '}
do
   expand $NAME|awk "{if (length() >= $LEN)  print \"$NAME:Line=\", NR,  \"Length=\", length(), \$0 }" 
done
#===============================================================================
exit
#===============================================================================
If no parameters are given, acts as a filter with length set to 72 (for Fortran77 programmers)
If parameters are given first one must be the line length limit

