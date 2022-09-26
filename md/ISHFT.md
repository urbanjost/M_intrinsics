## ishft

### **Name**

**ishft**(3) - \[BIT:SHIFT\] Shift bits

### **Synopsis**
```fortran
    result=ishftc( i, shift )
```
```fortran
     elemental integer(kind=KIND) function ishft(i, shift )

      integer(kind=KIND),intent(in) :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
```
  where KIND and  SHIFTKIND may be any supported _integer_ kind, but where
  the kind for **i** dictates the kind of the returned value.

### **Description**

  **ishft**(3) returns a value corresponding to **i** with all of the
  bits shifted **shift** places left or right as specified by the sign
  and magnitude of **shift**.

  Bits shifted out from the left end or right end are lost; zeros are
  shifted in from the opposite end.

### **Options**

- **i**
  : The value specifying the pattern of bits to shift

- **shift**
  : A value of **shift** greater than zero corresponds to a left shift,
  a value of zero corresponds to no shift, and a value less than zero
  corresponds to a right shift.

  If the absolute value of **shift** is
  greater than **bit_size(i)**, the value is undefined.

### **Result**

  The return value has the same characteristics (shape, kind, ...)  as  **i**.

### **Examples**

Sample program:
```fortran
program demo_ishft
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer             :: shift
integer             :: i
character(len=*),parameter :: g='(b32.32,1x,i0)'

   write(*,*) ishft(3, 1),' <== typically should have the value 6'

   shift=4
   write(*,g) ishft(huge(0),shift), shift
   shift=0
   write(*,g) ishft(huge(0),shift), shift
   shift=-4
   write(*,g) ishft(huge(0),shift), shift
end program demo_ishft
```
  Results:
```text
>              6  <== typically should have the value 6
>   11111111111111111111111111110000 4
>   01111111111111111111111111111111 0
>   00000111111111111111111111111111 -4
```
### **Standard**

Fortran 95 and later

### **See Also**

[**ishftc**(3)](#ishftc)

 _fortran-lang intrinsic descriptions_
