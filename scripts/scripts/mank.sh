#!/bin/bash
####################################################################################################################################
#@(#) mank(1) - build table of contents of man pages as an html page.
# uses footer.sh and header.sh
####################################################################################################################################
RANDOMCOLOR(){
# generate a random light background color 
# so each grouping has a color in the table
printf -v KOLOR '#%2.2x%2.2x%2.2x' $((RANDOM % 64+3*64)) $((RANDOM % 64+3*64)) $((RANDOM % 64+3*64))
}
####################################################################################################################################
MAKE_MANK(){
SECTION=${1:-3}
#----------------------------------------------------------------------------------------------------------------------------------#
header.sh # create header for document including CSS style
#----------------------------------------------------------------------------------------------------------------------------------#
cat <<EOF
<a href="https://github.com/urbanjost/M_intrinsics">[UP]</a>
<h1>$DOCUMENT_HEADER</h1>
EOF
#----------------------------------------------------------------------------------------------------------------------------------#
cat <<\EOF
<table border="1">
<tr> <th>grouping</th> <th>page</th> <th>description</th> </tr>
EOF
#----------------------------------------------------------------------------------------------------------------------------------#
export OLDGROUP GROUP KOLOR
OLDGROUP='&nbsp;'
RANDOMCOLOR
#----------------------------------------------------------------------------------------------------------------------------------#
#
# sort by 'member' assuming making lines for a module member using this syntax:
#    NAME (section) - [member] description
# example:
#    M_Compare_Float_Numbers (3) - [M_Compare_Float_Numbers]perform relational comparisons on real numbers
# but still work with regular lines like
#    _pwd (1)        - list full pathname of current directory
#
DELIMITER=$(printf '\t')

# make sure sort(1) does not sort case-insensitive
export LC_ALL=C 
man -S $SECTION -k . |
   eval $FILTER |
   sed -e "s/\[/$DELIMITER[/" |
   sed -e "s/]/]$DELIMITER/" |
   env LC_ALL=C sort -s -t "$DELIMITER" -k 2,2 -k 1,1 |
   tr -d "$DELIMITER" |
while read NAME SECT DASH OTHER
do
   GROUP=${OTHER/\]*/} GROUP=${GROUP/*\[/} GROUP=${GROUP:-'&nbsp;'}
   if [ "$OLDGROUP" != "$GROUP" ]
   then
      OLDGROUP="$GROUP"
      RANDOMCOLOR
   fi
   
   # will truncate description if description has ] not as I expect
   case "$OTHER" in
   *\]*) 
      IFS=']'
      set $OTHER
      shift
      OTHER=${*/*\]/}
   unset IFS
   ;;
   *) GROUP='&nbsp;' ;;
   esac
   SECT=$(echo "$SECT"|tr -d ')(')

   echo "<tr><td style=\"background:$KOLOR;\">$GROUP</td><td><a href=\"$NAME.$SECT.html\">$NAME</a></td><td>$OTHER</td></tr>"
done
#----------------------------------------------------------------------------------------------------------------------------------#
cat <<\EOF
</table>
EOF
#----------------------------------------------------------------------------------------------------------------------------------#
footer.sh
}
####################################################################################################################################
#                          i                 
#                                            
# mmmm mmm     aaaa        i       n nnnnn   
# m   m   m        a       i       nn     n  
# m   m   m   aaaaaa       i       n      n  
# m   m   m  a     a       i       n      n  
# m   m   m   aaaaa a      i       n      n  
#
####################################################################################################################################
PATH=$PATH:$(dirname $0)
export WHERE=../docs
export MANPATH=../man
#----------------------------------------------------------------------------------------------------------------------------------E
# index skipping anything without '[FORTRAN' in the description
export FILTER="grep '\[.*\].*\]'"
export FILTER="cat"
for SUBDIR in 3 5 7
do
   echo "making HTML index for Fortran Intrinsics for section $SUBDIR in $WHERE/man${SUBDIR}i.html" # 1>&2
   DOCUMENT_HEADER="man(${SUBDIR}) pages for Fortran Intrinsics"
   MAKE_MANK "${SUBDIR}" > $WHERE/index$SUBDIR.html
done
# remove content index from html documents
sed -i -e '/HREF=#/d' -e '/name=contents/d' $WHERE/*.[357]fortran.html
####################################################################################################################################
exit
####################################################################################################################################
