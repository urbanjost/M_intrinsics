## is_iostat_end

### **Name**

**is_iostat_end**(3) - \[STATE:INQUIRY\] Test for end-of-file value

### **Synopsis**
```fortran
    result = is_iostat_end(iostat)
```
```fortran
     elemental logical function is_iostat_end(iostat)

      integer,intent(in) :: iostat
```
### **Characteristics**

### **Description**

**is_iostat_end**(3) tests whether a variable (assumed returned as a status
from an I/O statement) has the "end of file" I/O status value.

The function is equivalent to comparing the variable with the
**iostat_end** parameter of the intrinsic module **iso_fortran_env**.

### **Options**

- **i**
  : An _integer_ status value to test if indicating end of file.

### **Result**

Returns a _logical_ of the default kind, _.true._ if **i** has the value
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
      endif
      !
   enddo
end program demo_iostat
```
### **Standard**

Fortran 2003

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
