## shiftl

### **Name**

**shiftl**(3) - \[BIT:SHIFT\] Shift bits left

### **Synopsis**
```fortran
    result = shiftl( i, shift )
```
```fortran
     elemental integer(kind=KIND) function shiftl(i, shift)

      integer(kind=KIND),intent(in) :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
```
### **Characteristics**

where KIND and SHIFTKIND may be any supported _integer_ kind, but
where the kind for **i** dictates the kind of the returned value.

### **Description**

Returns a value corresponding to **i** with all of the bits shifted left
by **shift** places.

Bits shifted out from the left end are lost, and bits shifted in from
the right end are set to **0**.

If the absolute value of **shift** is greater than **bit_size(i)**,
the value is undefined.

Note the value of the result is the same as **ishft (i, shift)**.

### **Options**

- **i**
  : The initial value to shift and fill in with zeros

- **shift**
  : how many bits to shift left.
    It shall be nonnegative and less than or equal to **bit_size(i)**.

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Examples**

Sample program:
```fortran
program demo_shiftl
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer             :: shift
integer(kind=int32) :: oval
integer(kind=int32) :: ival
integer(kind=int32),allocatable :: ivals(:)
integer             :: i

 ! basic usage
  ival=100
  write(*,*)ival, shiftl(100,3)

 ! elemental (input values may be conformant arrays)
  ! shifting is often equivalent to multiplying be a power of two
  write(*,*) shiftl(-1,[(i,i=1,bit_size(0))])
  write(*,*)
  write(*,*) shiftl(+3,[(i,i=1,bit_size(0))])

 ! loop through some ivalues
   shift=9
   ivals=[ &
   & int(b"01010101010101010101010101010101"), &
   & int(b"10101010101010101010101010101010"), &
   & int(b"11111111111111111111111111111111") ]

   write(*,'(/,"SHIFT =  ",i0)') shift
   do i=1,size(ivals)
      ! print initial value as binary and decimal
      write(*,'(  "I =      ",b32.32," == ",i0)') ivals(i),ivals(i)
      ! print shifted value as binary and decimal
      oval=shiftl(ivals(i),shift)
      write(*,'(  "RESULT = ",b32.32," == ",i0)') oval,oval
   enddo

  ! elemental (input values may be conformant arrays)
   ELEM : block
   integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])
   write(*,*)"characteristics of the result are the same as input"
   write(*,'(*(g0,1x))') &
     & "kind=",kind(shiftl(arr,3)), "shape=",shape(shiftl(arr,3)), &
     & "size=",size(shiftl(arr,3)) !, "rank=",rank(shiftl(arr,3))
   endblock ELEM

end program demo_shiftl
```
  Results:
```text
>         100         800
>
>           -2          -4          -8         -16         -32         -64
>         -128        -256        -512       -1024       -2048       -4096
>        -8192      -16384      -32768      -65536     -131072     -262144
>      -524288    -1048576    -2097152    -4194304    -8388608   -16777216
>    -33554432   -67108864  -134217728  -268435456  -536870912 -1073741824
>  -2147483648           0
>
>            6          12          24          48          96         192
>          384         768        1536        3072        6144       12288
>        24576       49152       98304      196608      393216      786432
>      1572864     3145728     6291456    12582912    25165824    50331648
>    100663296   201326592   402653184   805306368  1610612736 -1073741824
>  -2147483648           0
>
>   SHIFT =  9
>   I =      01010101010101010101010101010101 == 1431655765
>   RESULT = 10101010101010101010101000000000 == -1431655936
>   I =      10101010101010101010101010101010 == -1431655766
>   RESULT = 01010101010101010101010000000000 == 1431655424
>   I =      11111111111111111111111111111111 == -1
>   RESULT = 11111111111111111111111000000000 == -512
>    characteristics of the result are the same as input
>   kind= 1 shape= 2 2 size= 4
```

### **Standard**

Fortran 2008

### **See Also**

[**shifta**(3)](#shifta),
[**shiftr**(3)](#shiftr),
[**ishft**(3)](#ishft),
[**ishftc**(3)](#ishftc)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
