#!/bin/bash
####################################################################################################################################
# make HTML slides page
MAKE_SLIDY(){
cd md
FILES=$(
   for NAME in *.md
   do
   case "$NAME" in
   index.md);;
   *_index.md);;
   GNU*);;
   *)echo "$NAME" ;;
   esac
   done
)
echo FILES $FILES|xargs -n 5|column -t

echo "creating docs/intrinsics.md" 1>&2
for NAME in $FILES
do
   echo "# $( basename ${NAME} .md)"|sed -e 's/_/\\_/g'
   # bug when label contains underscore
   #echo "# $( basename ${NAME} .md)"|sed -e 's/_//g'
   # bug in some conversions if no blank line above a section
   echo ""

   sed -n -e '\%^##%,${ p }'  $NAME |
   # remove trailing whitespace
   sed -e 's/ *$//' |
   # delete blank lines at top of file
   awk 'NF {f=1} f' |
   # get rid of artifact from converting from markdown
   grep -v '^-$' |
   expand |
   cat -s
done >../docs/intrinsics.md

echo "creating docs/intrinsics_slidy.html" 1>&2
pandoc -t slidy  --metadata title="Fortran Intrinsics" -s "../docs/intrinsics.md" \
   --standalone --slide-level=1 -o ../docs/intrinsics_slidy.html
}
MAKE_SLIDY
