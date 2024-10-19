## huge

### **Name**

**huge**(3) - \[MODEL:NUMERIC\] Largest number of a type and kind

### **Synopsis**
```fortran
    result = huge(x)
```
```fortran
     TYPE(kind=KIND) function huge(x)

      TYPE(kind=KIND),intent(in) :: x(..)
```
### **Characteristics**

 - **x** may be any _real_ or _integer_ scalar or array and any kind.
 - The result will be a scalar of the same type and kind as the input **x**

### **Description**

  **huge**(3) returns the largest number that is not an overflow
  for the kind and type of **x**.

### **Options**

- **x**
  : **x** is an arbitrary value which is used merely to determine what
  _kind_ and _type_ of scalar is being queried. It need not be defined,
  as only its characteristics are used.

### **Result**

  The result is the largest value supported by the specified type
  and kind.

  Note the result is as the same kind as the input to ensure the returned
  value does not overflow. Any assignment of the result to a variable
  requires the variable must be able to hold the value as well. For
  example:
```fortran
     real :: r
     r=huge(0.0d0)
```
  where R is single-precision would almost certainly result in overflow.

### **Examples**

Sample program:
```fortran
program demo_huge
implicit none
character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
integer                    :: i, j, k, biggest
real                       :: v, w
doubleprecision            :: tally
   ! basic
   print *, huge(0), huge(0.0), huge(0.0d0)
   print *, tiny(0.0), tiny(0.0d0)

   tally=0.0d0
   ! note subtracting one because counter is the end value+1 on exit
   do i=0,huge(0)-1
      tally=tally+i
   enddo
   write(*,*)'tally=',tally

   ! advanced
   biggest=huge(0)
   ! be careful of overflow when using integers in computation
   do i=1,14
      j=6**i   ! Danger, Danger
      w=6**i   ! Danger, Danger
      v=6.0**i
      k=v      ! Danger, Danger

      if(v.gt.biggest)then
         write(*,f) i, j, k, v, v.eq.w, 'wrong j and k and w'
      else
         write(*,f) i, j, k, v, v.eq.w
      endif
   enddo
   ! a simple check of the product of two 32-bit integers
   print *,checkprod([2,4,5,8],[10000,20000,3000000,400000000])

contains
impure elemental function checkprod(i,j) result(ij32)
! checkprod(3f) - check for overflow when multiplying 32-bit integers
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
integer(kind=int32),intent(in)  :: i, j
integer(kind=int64)             :: ij64
integer(kind=int32)             :: ij32
integer,parameter               :: toobig=huge(0_int32)
character(len=80)               :: message
   ij64=int(i,kind=int64)*int(j,kind=int64)
   if(ij64.gt.toobig)then
      write(message,'(*(g0))')&
      & '<ERROR>checkprod(3f):',i,'*',j,'=',ij64,'>',toobig
      stop message
   else
      ij32=ij64
   endif
end function checkprod
end program demo_huge
```
Results:
```text
 >   2147483647   3.40282347E+38   1.7976931348623157E+308
 >    1.17549435E-38   2.2250738585072014E-308
 >  tally=   2.3058430049858406E+018
 >  1           6           6             6. T
 >  2          36          36            36. T
 >  3         216         216           216. T
 >  4        1296        1296          1296. T
 >  5        7776        7776          7776. T
 >  6       46656       46656         46656. T
 >  7      279936      279936        279936. T
 >  8     1679616     1679616       1679616. T
 >  9    10077696    10077696      10077696. T
 > 10    60466176    60466176      60466176. T
 > 11   362797056   362797056     362797056. T
 > 12 -2118184960 -2147483648    2176782336. F wrong j and k and w
 > 13   175792128 -2147483648   13060694016. F wrong j and k and w
 > 14  1054752768 -2147483648   78364164096. F wrong j and k and w
 > STOP <ERROR>checkprod(3f):8*400000000=3200000000>2147483647
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
