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
real               :: value
integer            :: ios
character(len=256) :: message
   write(*,*)'Begin entering numeric values, one per line'
   do
      read(*,*,iostat=ios,iomsg=message)value
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
end program demo_iostat
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
