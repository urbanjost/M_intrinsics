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
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32,real64,real128
implicit none
character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
integer                    :: i, j, k, biggest
real                       :: v, w
integer,allocatable        :: undef(:,:,:)

   print *,'basics:'

   print *, huge(0), huge(0.0), huge(0.0d0)
   print *, tiny(0.0), tiny(0.0d0)
   print *, 'an array argument returns a scalar'
   print *, huge([10_int8,20_int8,30_int8])
   print *, 'the value of the argument does not matter, it does not'
   print *, 'even need to be allocated, just the type and kind are'
   print *, 'used',huge(1000),huge(-654321),huge(undef)

   print *, 'dusty corners:'

   print *, 'Perhaps instead of an "infinite" loop you want to make'
   print *, 'a very large one so you have a counter handy.'
   do i=1,huge(0)-1
      call random_number(w)
      if(w > 0.9999999)exit
   enddo
   write(*,*)'exited with counter=',i
   ! use huge(0)-1 not huge(0) because when a loop terminates normally
   ! the counter is set to the last value + 1. If the loop reached
   ! i=huge(0) adding 1 would cause an overflow!

   ! Can HUGE(1.d0) be accurately formatted?
   print '(E330.320)', huge(1.d0)
   print *, huge(1.d0)
   print '(g0)', huge(1.d0)

   print *,'ranges for signed numbers  are symmetrical so if HUGE(0.0)'
   print *,'is a valid number  so is -HUGE(0.0).'
   print *, huge(0.0),-huge(0.0), huge(0.0)-huge(0.0)
   print *,'but for 2''s-complement whole numbers -1-huge(0)='
   print *,  -1-huge(0)
   print *,'is a valid number too, but huge(0)+1 will cause an overflow!'
   print *,'Almost all computers use 2''s-complement integers now-adays.'
   print *,'so -huge(0)-1 is often used as a "magic number" to designate'
   print *,'invalid whole numbers, as INTEGER types do not have a Nan'
   print *,'or Infinite value like floats do if it is not a "possible"'
   print *,'value for a computation.'
   print *
   print *,'for a single byte a value can be from -128 to 127 so maybe'
   print *,'-128 is not unlikely to be used though, for example:'
   print *,'range of a 2''scomplement one-byte kind is',-huge(0_int8)-1,&
         & 'to',huge(0_int8)
   print *,'so there is no "perfect" integer value to represent an '
   print *,'invalid number except on a case-by-case basis.'

   print *,'advanced:'

   print *,'be careful of overflow; Fortran is not required to report it'
   print *,'See OUT_OF_RANGE(3) for information on detecting overflows.'

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
 >  basics:
 >   2147483647   3.40282347E+38   1.7976931348623157E+308
 >    1.17549435E-38   2.2250738585072014E-308
 >  an array argument returns a scalar
 >   127
 >  the value of the argument does not matter, it does not
 >  even need to be allocated, just the type and kind are
 >  used  2147483647  2147483647  2147483647
 >  dusty corners:
 >  Perhaps instead of an "infinite" loop you want to make
 >  a very large one so you have a counter handy.
 >  exited with counter=      851169
 >     0.17976931348623157081452742373170435679807056752584499
 >     6598917476803157260780028538760589558632766878171540458
 >     9535143824642343213268894641827684675467035375169860499
 >     1057655128207624549009038932894407586850845513394230458
 >     3236903222948165808559332123348274797826204144723168738
 >     17718091929988125040402618412485836800000000000+309
 >    1.7976931348623157E+308
 > 0.17976931348623157E+309
 >  ranges for signed numbers  are symmetrical so if HUGE(0.0)
 >  is a valid number  so is -HUGE(0.0).
 >    3.40282347E+38  -3.40282347E+38   0.00000000
 >  but for 2's-complement whole numbers -1-huge(0)=
 >  -2147483648
 >  is a valid number too, but huge(0)+1 will cause an overflow!
 >  Almost all computers use 2's-complement integers now-adays.
 >  so -huge(0)-1 is often used as a "magic number" to designate
 >  invalid whole numbers, as INTEGER types do not have a Nan
 >  or Infinite value like floats do if it is not a "possible"
 >  value for a computation.
 >
 >  for a single byte a value can be from -128 to 127 so maybe
 >  -128 is not unlikely to be used though, for example:
 >  range of a 2'scomplement one-byte kind is        -128 to  127
 >  so there is no "perfect" integer value to represent an
 >  invalid number except on a case-by-case basis.
 >  advanced:
 >  be careful of overflow; Fortran is not required to report it
 >  See OUT_OF_RANGE(3) for information on detecting overflows.
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

### **Trivia**

The Fortran standard does not specify what the largest value can be in
a "Ew.d" edit descriptor, so "E330.320" will likely generate a value
that fills all the positions without an error, or maybe cap it at some
number of digits even though the vast majority will not be significant.

But perhaps the bigger surprise is the same insignificant digits will
likely be generated across platforms.

Unless the platform chooses to pad with zeros it is likely a platform
generates the number shown above, not random digits once it gets past
the few significant digits.

Most compilers print identical digits because IEEE 754 double
precision defines an exact binary bit pattern for "HUGE(0.D0)", and
runtime libraries use standard, deterministic conversion algorithms to
translate those exact bits into decimal text. Trailing digits beyond
the 17th significant figure are purely algorithmic padding or repeating
representations.

#### IEEE 754 Binary Representation

  + The value "HUGE(0.D0)" in Fortran represents the maximum positive
    finite double-precision number (1.7976931348623157 × 10\*\*308).
  + Internally, this is stored as a fixed 64-bit binary floating-point
    number with a 53-bit significand (mantissa) and an 11-bit exponent.
  + Because the underlying binary value is identical across all
    conforming hardware and compilers, the exact mathematical value
    being converted is always the same.

#### Decimal Conversion and Padding

  + A standard double-precision number only contains about 15 to 17
    significant decimal digits of true numerical precision.
  + When you request 320 digits of precision using "E330.320", the
    runtime formatting library runs out of real bits from the binary
    number very quickly.
  + To fulfill the large requested width, the compiler's runtime library
    either pads the remaining lower-order digits with deterministic
    zeros, or it mathematically exposes the repeating/terminating
    behavior of the binary-to-decimal floating-point conversion routine
    (such as Grisu or Ryu algorithms). Because different modern
    compilers often link against similar standard math/io library
    implementations or follow the exact same IEEE decimal formatting
    specifications, the extended trailing digits match precisely.
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
