      6    6 Examples. DSHIFTR (1, 16, 3) has the value 2        +2if default integer has 32 bits.
## dshiftr

### **Name**

**dshiftr**(3) - \[BIT:COPY\] combined right shift of the bits of two integers

### **Syntax**

```fortran
elemental integer(kind=KIND) function dshiftr(i, j, shift)

 integer(kind=KIND),intent(in) :: i
 integer(kind=KIND),intent(in) :: j
 integer(kind=KIND2),intent(in) :: shift
```
  Where the kind of **i**, **j**, and **dshiftr** are the same.  An
  exception is that one of **i** and **j** may be a BOZ literal constant.

### **Description**

**dshiftr(i, j, shift)** combines bits of **i** and **j**. The leftmost **shift**
bits of the result are the rightmost **shift** bits of **i**, and the remaining
bits are the leftmost bits of **j**.

This is equivalent to
```fortran
     ior(shiftl (i, bit_size(i)−shift), shiftr(j, shift) )
```
It may be thought of as appending the bits of **i** and **j**, dropping off the
**shift** rightmost bits, and then retaining the same number of rightmost bits
as an input value, hence the name "combined right shift"...

```text
GIven two 16-bit values labeled alphabetically ...

   i=ABCDEFGHIJKLMNOP
   j=abcdefghijklmnop

Append them together

   ABCDEFGHIJKLMNOPabcdefghijklmnop

Shift them N=6 bits to the right dropping off bits

   ......ABCDEFGHIJKLMNOPabcdefghij

Keep the 16 right-most bits

   KLMNOPabcdefghij
```
Pictured this way it can be seen that if **i** and **j** have the same
value 
```fortran
     dshiftr( i, i, shift )
```
this has the same result as a negative circular shift
```fortran
     ishftc( i, −shift ).
```

### **Arguments**

- **i**
  : Shall be of type _integer_.

- **j**
  : Shall be of type _integer_, and of the same kind as **i**.

- **shift**
  : Shall be of type _integer_.
    It shall be nonnegative and less than or equal to **bit_size(result)** 
    where "result" is the _integer_ kind of the returned value/input integers.

### **Returns**

The return value has same type and kind as **i**.

### **Examples**

Sample program:

```fortran
program demo_dshiftr
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer(kind=int32) :: i, j
integer             :: shift

  ! basic usage
   write(*,*) dshiftr (1, 2**30, 2) 

  ! print some calls as binary to better visualize the results
   i=-1
   j=0
   shift=5
   call printit()

  ! visualizing a "combined right shift" ...
   i=b"00000000000000000000000000011111"
   j=b"11111111111111111111111111100000"
   ! appended together ( i//j )
   ! 0000000000000000000000000001111111111111111111111111111111100000
   ! shifted right SHIFT values dropping off shifted values
   ! .....00000000000000000000000000011111111111111111111111111111111
   ! keep enough rightmost bits to fill the kind
   ! 11111111111111111111111111111111
   ! so the result should be all 1s bits ...
   call printit()

contains 
subroutine printit()
   ! print i,j,shift and then i,j, and the result as binary values
    write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
    write(*,'(b32.32)') i,j, dshiftr (i, j, shift)
end subroutine printit

end program demo_dshiftr
```
  Results:
```> text
   >   1342177280
   > I=-1 J=0 SHIFT=5
   > 11111111111111111111111111111111
   > 00000000000000000000000000000000
   > 11111000000000000000000000000000
   > I=31 J=-32 SHIFT=5
   > 00000000000000000000000000011111
   > 11111111111111111111111111100000
   > 11111111111111111111111111111111
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**dshiftl**(3)](#dshiftl)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
