## is_iostat_end

### **Name**

**is_iostat_end**(3) - \[STATE:INQUIRY\] Test for end-of-file value

### **Synopsis**
```fortran
    result = is_iostat_end(i)
```
```fortran
     elemental logical function is_iostat_end(i)

      integer,intent(in) :: i
```
### **Characteristics**

 - **i** is _integer_ of any kind
 - the return value is a default _logical_

### **Description**

**is_iostat_end**(3) tests whether a variable (assumed returned as a status
from an I/O statement) has the "end of file" I/O status value.

The function is equivalent to comparing the variable with the
**iostat_end** parameter of the intrinsic module **iso_fortran_env**.

### **Options**

- **i**
  : An _integer_ status value to test if indicating end of file.

### **Result**

returns _.true._ if and only if**i** has the value
which indicates an end of file condition for **iostat=** specifiers, and is
_.false._ otherwise.

### **Examples**

Sample program:

```fortran
program demo_iostat
implicit none
integer,parameter  :: wp=kind(0.0d0)
real(kind=wp)      :: value
integer            :: ios
integer            :: lun
character(len=256) :: message
   ! make a scratch input file for demonstration purposes
   call makefile(lun) 
   write(*,*)'Begin entering numeric values, one per line'
   do
      read(lun,*,iostat=ios,iomsg=message)value
      if(ios.eq.0)then
         write(*,*)'VALUE=',value
      elseif( is_iostat_end(ios) ) then
         stop 'end of file. Goodbye!'
      else
         write(*,*)'ERROR:',ios,trim(message)
         exit
      endif
      !
   enddo
contains
subroutine makefile(lun)
! make a scratch file just for demonstration purposes
integer :: iostat,lun
integer :: i
character(len=255),parameter  :: fakefile(*)=[character(len=255) :: &

'3.141592653589793238462643383279502884197169399375105820974944592307 &
 &/ pi', &

'0.577215664901532860606512090082402431042 &
 &/ The Euler-Mascheroni constant (Gamma)', &

'2.71828182845904523536028747135266249775724709369995 &
 &/ Napier''s constant "e"&
 & is the base of the natural logarithm system,&
 & named in honor of Euler ', &

'1.6180339887498948482045868 &
 &/ Golden_Ratio', &

'1 / unity', &
''] 
!'/ end of data']

   open(newunit=lun,status='replace',file='data.txt',action='readwrite')
   write(lun,'(a)')(trim(fakefile(i)),i=1,size(fakefile))
   rewind(lun)
end subroutine makefile
end program demo_iostat
```
Results:
```text
 >  Begin entering numeric values, one per line
 >  VALUE=   3.1415926535897931     
 >  VALUE=  0.57721566490153287     
 >  VALUE=   2.7182818284590451     
 >  VALUE=   1.6180339887498949     
 >  VALUE=   1.0000000000000000     
 >  STOP end of file. Goodbye!
```
### **Standard**

Fortran 2003

### **See also**

 - [associated(3)](#associated) -  Association status of a pointer or pointer/target pair
 - [extends_type_of(3)](#extends_type_of) -  Determine if the dynamic type of A is an extension of the dynamic type of MOLD.
 - [is_iostat_end(3)](#is_iostat_end) -  Test for end-of-file value
 - [is_iostat_eor(3)](#is_iostat_eor) -  Test for end-of-record value
 - [present(3)](#present)   -  Determine whether an optional dummy argument is specified
 - [same_type_as(3)](#same_type_as) -  Query dynamic types for equality

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
