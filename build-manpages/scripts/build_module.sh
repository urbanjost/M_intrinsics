#!/bin/bash
export NAME
###########################
TOCHARACTER(){
:
cat $NAME |
   sed -e "s/'/''/g" |
   sed -e "s/^/'/" |
   sed -e 's/$/'"'"', \&/'|
cat
}
###########################
(
cd $(dirname $0)
cd ../../md
ls *.*fortran.man|while read NAME
do
SHORTNAME=${NAME/.*}
(
cat <<EOF
function help_${SHORTNAME}() result (textblock)
character(len=:),allocatable   :: textblock(:)
textblock=[character(len=132) :: &
'', &
$(TOCHARACTER)
'']
end function help_${SHORTNAME}
EOF
)

done) > /dev/null # help_names.f90
###########################
(
cd $(dirname $0)
cd ../../md
cat <<\EOF
module M_help_intrinsics
implicit none
private
public help_intrinsics
!interface help_intrinsics
!   module procedure help_intrinsics_all
!   module procedure help_intrinsics_one
!end interface help_intrinsics
contains
function help_intrinsics(name) result (textblock)
character(len=*),intent(in)    :: name
character(len=:),allocatable  :: textblock(:)
    if(name.eq.'')then
       textblock=help_intrinsics_all()
    else
       textblock=help_intrinsics_one(name)
    endif
end function help_intrinsics

function help_intrinsics_all() result (textblock)
character(len=132),allocatable  :: textblock(:)
character(len=132),allocatable  :: add(:)
character(len=10)               :: cnum
integer                         :: icount
    allocate(textblock(0))
    icount=1
    do
        write(cnum,'(i0)') icount
	add=help_intrinsics_one(cnum)
        if( size(add) .eq. 0 ) exit
	textblock=[character(len=132) :: textblock,add]
        icount=icount + 1
    enddo
end function help_intrinsics_all

function help_intrinsics_one(name) result (textblock)
character(len=*),intent(in)    :: name
character(len=:),allocatable   :: textblock(:)
select case(name)
EOF
COUNT=0
ls *.*fortran.man|while read NAME
do
   SHORTNAME=${NAME/.*}
   COUNT=$((COUNT+1))
   cat <<EOF

case('$COUNT','$SHORTNAME') 

textblock=[character(len=132) :: &
'', &
$(TOCHARACTER)
'']
EOF
done
   cat <<\EOF
case default
    allocate (character(len=132) :: textblock(0))
end select
end function help_intrinsics_one

end module M_help_intrinsics
EOF
) #> M_help_intrinsics.f90
###########################
exit
###########################
