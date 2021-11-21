#!/bin/bash
pandoc --list-input-formats|xargs -n 5|column -t
echo =======================================================
pandoc --list-output-formats|xargs -n 5|column -t
echo =======================================================
# -mindepth 1 -maxdepth 2 
# %f %h %p %P
mkdir -p gfm
find man/man* -type f -name '*.*fortran' -printf '%p\n' |
while read NAME
do
   echo "$NAME"
   pandoc --from man --to gfm < $NAME > gfm/$(basename $NAME).md
done
exit
