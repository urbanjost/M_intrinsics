## dshiftl

### **Name**

**dshiftl**(3) - \[BIT:COPY\] combined left shift of the bits of two integers

### **Syntax**

```fortran
elemental integer(kind=KIND) function dshiftl(i, j, shift)

 integer(kind=KIND),intent(in) :: i
 integer(kind=KIND),intent(in) :: j
 integer(kind=KIND2),intent(in) :: shift
```
  Where the kind of **i**, **j**, and **dshiftl** are the same.  An
  exception is that one of **i** and **j** may be a BOZ literal constant.

### **Description**

  **dshiftl(i, j, shift)** combines bits of **i** and **j**. Per the
  standard the rightmost **shift** bits of the result are the leftmost
  **shift** bits of **j**, and the remaining bits are the rightmost bits
  of **i**.

  For example, for 32-bit values if **shift=6** designating ignored
  bits with "-" and labeling the used bits of **i** with uppercase
  letters and used bits of **j** with lowercase letters the result
  would be ...
```text
      SHIFT=6
      I =      ------ABCDEFGHIJKLMNOPQRSTUVWXYZ
      J =      abcdef--------------------------
      RESULT = ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef
```
  So reading from left to right we skip the first N values of **i**
  and use the first N values of **j** and append them together.

  This is equivalent to
```fortran
     ior( shiftl(i, shift), shiftr(j, bit_size(j)−shift) )
```
  hence **dshiftl** is designated as a "combined left shift", because
  it is like we appended **i** and **j** together, shifted it **shift**
  bits to the left, and then kept the same number of bits as **i** or
  **j** had. Using the above strings:
```text
   Combine them together
      ------ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef--------------------------
   Shift 6 to the left
      ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef--------------------------
   keep 32 bits
      ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef
```

#### Note:
  Using the last representation of the operation is should be
  seen that when both **i** and **j** have the same value as in
```fortran
      dshiftl(i, i, shift)
```
  the result has the same value as a circular shift:
```fortran
      ishftc(i, shift)
```

### **Arguments**

- **i**
  : Shall be of type _integer_.

- **j**
  : Shall be of type _integer_, and of the same kind as **i**.

  If either **i** or **j** is a BOZ-literal-constant, it is ﬁrst
  converted as if by the intrinsic function **int()** to _integer_
  with the kind type parameter of the other.

- **shift**
  : Shall be of type _integer_.
    It shall be nonnegative and less than or equal to BIT_SIZE(K) where K is
    any **i** or **j** variable that is type _integer_ (ie. the size of either
    one that is not a BOZ literal constant).

### **Returns**

  The return value has same type and kind as **i** and/or **j**.

  The leftmost **shift** bits of **j** are copied to the rightmost bits
  of the result, and the remaining bits are the rightmost bits of **i**.

### **Examples**

Sample program:

```fortran
program demo_dshiftl
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer(kind=int32) :: i, j
integer             :: shift

  ! basic usage
   write(*,*) dshiftl (1, 2**30, 2) ! int32 values on little-endian => 5

  ! print some simple calls as binary to better visual the results
   i=-1
   j=0
   shift=5
   call printit()

   ! the leftmost SHIFT bits of J are copied to the rightmost result bits
   j=b"11111000000000000000000000000000"
   ! and the other bits are the rightmost bits of I
   i=b"00000000000000000000000000000000"
   call printit()

   j=b"11111000000000000000000000000000"
   i=b"00000111111111111111111111111111"
   ! result should be all 1s
   call printit()

contains
subroutine printit()
   ! print i,j,shift and then i,j, and the result as binary values
    write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
    write(*,'(b32.32)') i,j, dshiftl (i, j, shift)
end subroutine printit

end program demo_dshiftl
```

Results:

```text
   > I=-1 J=0 SHIFT=5
   > 11111111111111111111111111111111
   > 00000000000000000000000000000000
   > 11111111111111111111111111100000
   > I=0 J=-134217728 SHIFT=5
   > 00000000000000000000000000000000
   > 11111000000000000000000000000000
   > 00000000000000000000000000011111
   > I=134217727 J=-134217728 SHIFT=5
   > 00000111111111111111111111111111
   > 11111000000000000000000000000000
   > 11111111111111111111111111111111
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**dshiftr**(3)](#dshiftr)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
