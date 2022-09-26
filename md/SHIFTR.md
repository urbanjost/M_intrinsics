## shiftr

### **Name**

**shiftr**(3) - \[BIT:SHIFT\] shift bits right

### **Synopsis**
```fortran
    result = shiftr( i, shift )
```
```fortran
     elemental integer(kind=KIND) function shiftr(i, shift)

      integer(kind=KIND),intent(in) :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
```
where KIND and SHIFTKIND may be any supported _integer_ kind, but
where the kind for **i** dictates the kind of the returned value.

### **Description**

Returns a value corresponding to **i** with all of the bits shifted right
by **shift** places. If the absolute value of **shift** is greater than
**bit_size(i)**, the value is undefined. Bits shifted out from the right
end are lost, and bits shifted in from the left end are set to 0.

Note the value of the result is the same as **ishft (i, -shift)**.

### **Options**

- **i**
  : The initial value to shift and fill in with zeros

- **shift**
  : how many bits to shift right.
    It shall be nonnegative and less than or equal to **bit_size(i)**.

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Examples**

Sample program:
```fortran
program demo_shiftr
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer             :: shift
integer(kind=int32) :: oval
integer(kind=int32) :: ival
integer(kind=int32),allocatable :: ivals(:)
integer             :: i

  ! basic usage
  ival=100
  write(*,*)ival, shiftr(100,3)

  ! elemental (input values may be conformant arrays)
  write(*,*) shiftr(-1,[(i,i=1,bit_size(0))])

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
      oval=shiftr(ivals(i),shift)
      write(*,'(  "RESULT = ",b32.32," == ",i0)') oval,oval
   enddo

   ! more on elemental (input values may be conformant arrays)
   ELEM : block
   integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])
   write(*,*)"characteristics of the result are the same as input"
   write(*,'(*(g0,1x))') &
     & "kind=",kind(shiftr(arr,3)), "shape=",shape(shiftr(arr,3)), &
     & "size=",size(shiftr(arr,3)) !, "rank=",rank(shiftr(arr,3))
   endblock ELEM

end program demo_shiftr
```
  Results:
```text
>          100          12
>   2147483647  1073741823   536870911   268435455   134217727    67108863
>     33554431    16777215     8388607     4194303     2097151     1048575
>       524287      262143      131071       65535       32767       16383
>         8191        4095        2047        1023         511         255
>          127          63          31          15           7           3
>            1           0
>
>  SHIFT =  9
>  I =      01010101010101010101010101010101 == 1431655765
>  RESULT = 00000000001010101010101010101010 == 2796202
>  I =      10101010101010101010101010101010 == -1431655766
>  RESULT = 00000000010101010101010101010101 == 5592405
>  I =      11111111111111111111111111111111 == -1
>  RESULT = 00000000011111111111111111111111 == 8388607
>   characteristics of the result are the same as input
>  kind= 1 shape= 2 2 size= 4
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**shifta**(3)](#shifta),
[**shiftl**(3)](#shiftl),
[**ishft**(3)](#ishft),
[**ishftc**(3)](#ishftc)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
