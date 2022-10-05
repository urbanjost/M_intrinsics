
## abs

### **Name**

**abs**(3) - \[NUMERIC\] Absolute value

### **Synopsis**
```fortran
    result = abs(a)
```
```fortran
     elemental TYPE(kind=KIND) function abs(a)

      TYPE(kind=KIND),intent(in) :: a
```
### **Characteristics**

- **a** may be any _real_, _integer_, or _complex_ value.

- If **a** is _complex_ the returned value will be a _real_ with the
  same kind as **a**.

  Otherwise the returned type and kind is the same as for **a**.

### **Description**

   **abs**(3) computes the absolute value of numeric argument **a**.

   In mathematics, the absolute value or modulus of a real number **x**,
   denoted **|x|**, is the magnitude of **x** without regard to its sign.

   The absolute value of a number may be thought of as its distance from
   zero. So for a complex value the absolute value is a real number
   with magnitude **sqrt(x%re\*\*2,x%im\*\*2)**, as if the real component
   is the x value and the imaginary value is the y value for the point
   \<x,y\>.

### **Options**

- **a**
  : The value to compute the absolute value of.

### **Result**

   If **a** is of type _integer_ or _real_, the value of the result
   is the absolute value **|a|** and of the same type and kind as the
   input argument.

   If **a** is _complex_ with value **(x, y)**, the result is a _real_
   equal to a processor-dependent approximation to
```fortran
        sqrt(x**2 + y**2)
```
   computed without undue overflow or underflow (that means the
   computation of the result can overflow the allowed magnitude of the
   real value returned, and that very small values can produce underflows
   if they are squared while calculating the returned value, for example).

   That is, if you think of non-complex values as being complex values
   on the x-axis and complex values as being x-y points <x%re,x%im>
   the result of **abs**(3) is the (positive) magnitude of the distance
   of the value from the origin.

### **Examples**

Sample program:

```fortran
program demo_abs
implicit none
integer,parameter :: dp=kind(0.0d0)

integer           :: i = -1
real              :: x = -1.0
complex           :: z = (-3.0,-4.0)
doubleprecision   :: rr = -45.78_dp

character(len=*),parameter :: &
   ! some formats
   frmt  =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
   frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)',  &
   g     = '(*(g0,1x))'

  ! basic usage
    ! any integer, real, or complex type
    write(*, frmt)  'integer         ',  i, abs(i)
    write(*, frmt)  'real            ',  x, abs(x)
    write(*, frmt)  'doubleprecision ', rr, abs(rr)
    write(*, frmtc) 'complex         ',  z, abs(z)

  ! You can take the absolute value of any value whose positive value
  ! is representable with the same type and kind.
    write(*, *) 'abs range test : ', abs(huge(0)), abs(-huge(0))
    write(*, *) 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
    write(*, *) 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))
    ! A dusty corner is that abs(-huge(0)-1) of an integer would be
    ! a representable negative value on most machines but result in a
    ! positive value out of range.

  ! elemental
    write(*, g) ' abs is elemental:', abs([20,  0,  -1,  -3,  100])

  ! COMPLEX input produces REAL output
    write(*, g)' complex input produces real output', &
    & abs(cmplx(30.0_dp,40.0_dp,kind=dp))
    ! dusty corner: "kind=dp" is required or the value returned by
    ! CMPLX() is a default real instead of double precision

  ! the returned value for complex input can be thought of as the
  ! distance from the origin <0,0>
    write(*, g) ' distance of (', z, ') from zero is', abs( z )
    write(*, g) ' so beware of overflow with complex values'
    write(*, g) abs(cmplx( huge(0.0), huge(0.0) ))
    write(*, g) ' because the biggest default real is',huge(0.0)

end program demo_abs
```
Results:
```text
    integer          In: -1                     Out: 1
    real             In: -1.000000              Out: 1.000000
    doubleprecision  In: -45.78000000000000     Out: 45.78000000000000
    complex          In: (-3.000000,-4.000000)  Out: 5.000000
    abs range test :   2147483647  2147483647
    abs range test :   3.4028235E+38  3.4028235E+38
    abs range test :   1.1754944E-38  1.1754944E-38
    abs is elemental: 20 0 1 3 100
    complex input produces real output 50.00000000000000
    distance of ( -3.000000 -4.000000 ) from zero is 5.000000
    so beware of overflow with complex values
    Inf
    because the biggest default real is .3402823E+39
```
### **Standard**

   FORTRAN 77

### **See Also**

[**sign**(3)](#sign)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## achar

### **Name**

**achar**(3) - \[CHARACTER:CONVERSION\] Returns a character in a specified position in the ASCII collating sequence

### **Synopsis**
```fortran
    result = achar(i [,kind])
```
```fortran
     elemental character(len=1,kind=KIND) function achar(i,kind)

      integer(kind=**),intent(in) :: i
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type

- The _character_ kind returned is the value of **kind** if present.
  otherwise, a single default _character_ is returned.

### **Description**

**achar**(3) returns the character located at position **i** (commonly
called the _ADE_ or ASCII Decimal Equivalent) in the ASCII collating
sequence.

The **achar**(3) function is often used for generating in-band escape
sequences to control terminal attributes, as it makes it easy to
print unprintable characters such as escape and tab. For example:
```fortran
   write(*,'(*(a))')achar(27),'[2J'
```
will clear the screen on an ANSI-compatible terminal display,

### **Options**

- **i**
  : the _integer_ value to convert to an ASCII character, in the range
  0 to 127.
  : **achar**(3) shall have the value C for any character
  C capable of representation as a default character.

- **kind**
  : a _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

  Assuming **i** has a value in the range 0 <= I <= 127, the result is the
  character in position **i** of the ASCII collating sequence, provided
  the processor is capable of representing that character in the character
  kind of the result; otherwise, the result is processor dependent.

### **Examples**

Sample program:
```fortran
program demo_achar
use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
implicit none
integer :: i
   i=65
   write(*,'("decimal     =",i0)')i
   write(*,'("character   =",a1)')achar(i)
   write(*,'("binary      =",b0)')achar(i)
   write(*,'("octal       =",o0)')achar(i)
   write(*,'("hexadecimal =",z0)')achar(i)

   write(*,'(8(i3,1x,a,1x),/)')(i,achar(i), i=32,126)

   write(*,'(a)')upper('Mixed Case')
contains
! a classic use of achar(3) is to convert the case of a string

pure elemental function upper(str) result (string)
!
!$@(#) upper(3f): function to return a trimmed uppercase-only string
!
! input string to convert to all uppercase
character(*), intent(in)      :: str
! output string that contains no miniscule letters
character(len(str))           :: string
integer                       :: i, iend
integer,parameter             :: toupper = iachar('A')-iachar('a')
   iend=len_trim(str)
   ! initialize output string to trimmed input string
   string = str(:iend)
   ! process each letter in the string
   do concurrent (i = 1:iend)
       select case (str(i:i))
       ! located miniscule letter
       case ('a':'z')
          ! change miniscule to majuscule letter
          string(i:i) = achar(iachar(str(i:i))+toupper)
       end select
   enddo
end function upper
end program demo_achar
```
Results:
```
   decimal     =65
   character   =A
   binary      =1000001
   octal       =101
   hexadecimal =41
    32    33 !  34 "  35 #  36 $  37 %  38 &  39 '

    40 (  41 )  42 *  43 +  44 ,  45 -  46 .  47 /

    48 0  49 1  50 2  51 3  52 4  53 5  54 6  55 7

    56 8  57 9  58 :  59 ;  60 <  61 =  62 >  63 ?

    64 @  65 A  66 B  67 C  68 D  69 E  70 F  71 G

    72 H  73 I  74 J  75 K  76 L  77 M  78 N  79 O

    80 P  81 Q  82 R  83 S  84 T  85 U  86 V  87 W

    88 X  89 Y  90 Z  91 [  92 \  93 ]  94 ^  95 _

    96 `  97 a  98 b  99 c 100 d 101 e 102 f 103 g

   104 h 105 i 106 j 107 k 108 l 109 m 110 n 111 o

   112 p 113 q 114 r 115 s 116 t 117 u 118 v 119 w

   120 x 121 y 122 z 123 { 124 | 125 } 126 ~
   MIXED CASE
```

### **Note**

The ADEs (ASCII Decimal Equivalents) for ASCII are

```
*-------*-------*-------*-------*-------*-------*-------*-------*
| 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
| 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
| 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
| 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
| 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
| 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
| 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
| 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
| 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
| 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
| 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
| 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
| 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
|104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
|112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
|120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
*-------*-------*-------*-------*-------*-------*-------*-------*
```
### **Standard**

FORTRAN 77 , with KIND argument Fortran 2003

### **See Also**

[**char**(3)](#char),
[**iachar**(3)](#iachar),
[**ichar**(3)](#ichar)

### **Resources**

- [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [M_attr module](https://github.com/urbanjost/M_attr) for controlling ANSI-compatible terminals

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## acosh

### **Name**

**acosh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic cosine function

### **Synopsis**
```fortran
    result = acosh(x)
```
```fortran
     elemental TYPE(kind=KIND) function acosh(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_
 - **KIND** may be any kind supported by the associated type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

**acosh**(3) computes the inverse hyperbolic cosine of **x** in radians.

### **Options**

- **x**
  : The value to compute the hyperbolic cosine of

### **Result**

The result has a value equal to a processor-dependent approximation to
the inverse hyperbolic cosine function of X.

If **x** is _complex_, the imaginary part of the result is in radians
and lies between
```fortran
 0 <= aimag(acosh(x)) <= PI
```
### **Examples**

Sample program:
```fortran
program demo_acosh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ 1.0d0, 2.0d0, 3.0d0 ]
   write (*,*) acosh(x)
end program demo_acosh
```
Results:
```text
 0.000000000000000E+000   1.31695789692482        1.76274717403909
```
### **Standard**

Fortran 2008

### **See Also**
Inverse function: [**cosh**(3)](#cosh)

### **Resources**
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## acos

### **Name**

**acos**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arccosine (inverse cosine) function

### **Synopsis**
```fortran
    result = acos(x)
```
```fortran
     elemental TYPE(kind=KIND) function acos(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_
 - **KIND** may be any kind supported by the associated type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

**acos**(3) computes the arccosine of **x** (inverse of **cos(x)**).

### **Options**

- **x**
  : The value to compute the arctangent of.
  : If the type is _real_, the value must satisfy |**x**| <= 1.

### **Result**

The return value is of the same type and kind as **x**. The _real_ part of
the result is in radians and lies in the range **0 \<= acos(x%re) \<= PI** .

### **Examples**

Sample program:

```fortran
program demo_acos
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x , d2r

   ! basics
    x = 0.866_real64
    print all,'acos(',x,') is ', acos(x)

   ! acos(-1) should be PI
    print all,'for reference &
    &PI ~= 3.14159265358979323846264338327950288419716939937510'
    write(*,*) acos(-1.0_real64)
    d2r=acos(-1.0_real64)/180.0_real64
    print all,'90 degrees is ', d2r*90.0_real64, ' radians'
   ! elemental
    print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])
   ! complex
    print *,'complex',acos( (-1.0,  0.0) )
    print *,'complex',acos( (-1.0, -1.0) )
    print *,'complex',acos( ( 0.0, -0.0) )
    print *,'complex',acos( ( 1.0,  0.0) )

end program demo_acos
```
Results:
```text
 acos( 0.86599999999999999 ) is  0.52364958093182890
 for reference PI ~= 3.14159265358979323846264338327950288419716939937510
    3.1415926535897931
 90 degrees is  1.5707963267948966  radians
 elemental 3.14159274 2.09439516 1.57079637 1.04719758 0.00000000
  complex            (3.14159274,-0.00000000)
  complex             (2.23703575,1.06127501)
  complex             (1.57079637,0.00000000)
  complex            (0.00000000,-0.00000000)
```
### **Standard**

FORTRAN 77 ; for a _complex_ argument - Fortran 2008

### **See Also**
Inverse function: [**cos**(3)](cos)

### **Resources**
- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## adjustl

### **Name**

**adjustl**(3) - \[CHARACTER:WHITESPACE\] Left-justified a string

### **Synopsis**
```fortran
  result = adjustl(string)
```
```fortran
   elemental character(len=len(string),kind=KIND) function adjustl(string)

    character(len=*,kind=KIND),intent(in) :: string
```
### **Characteristics**
 - **string** is a _character_ variable of any supported kind
 - The return value is a _character_ variable of the same kind
   and length as **string**

### **Description**

  **adjustl**(3) will left-justify a string by removing leading
  spaces. Spaces are inserted at the end of the string as needed.

### **Options**

- **string**
  : the string to left-justify

### **Result**

  A copy of **string** where leading spaces are removed and the same
  number of spaces are inserted on the end of **string**.

### **Examples**

Sample program:
```fortran
program demo_adjustl
implicit none
character(len=20) :: str = '   sample string'
character(len=:),allocatable :: astr
integer :: length

   ! basic use
    write(*,'(a,"[",a,"]")') 'original: ',str
    str=adjustl(str)
    write(*,'(a,"[",a,"]")') 'adjusted: ',str

    ! a fixed-length string can be printed
    ! trimmed using trim(3f) or len_trim(3f)
    write(*,'(a,"[",a,"]")') 'trimmed:  ',trim(str)
    length=len_trim(str)
    write(*,'(a,"[",a,"]")') 'substring:',str(:length)

    ! note an allocatable string stays the same length too
    ! and is not trimmed by just an adjustl(3f) call.
    astr='    allocatable string   '
    write(*,'(a,"[",a,"]")') 'original:',astr
    astr = adjustl(astr)
    write(*,'(a,"[",a,"]")') 'adjusted:',astr
    ! trim(3f) can be used to change the length
    astr = trim(astr)
    write(*,'(a,"[",a,"]")') 'trimmed: ',astr

end program demo_adjustl
```
Results:
```text
   original: [   sample string    ]
   adjusted: [sample string       ]
   trimmed:  [sample string]
   substring:[sample string]
   original:[    allocatable string   ]
   adjusted:[allocatable string       ]
   trimmed: [allocatable string]
```
### **Standard**

Fortran 95

### **See Also**

[**adjustr**(3)](#adjustr),
[**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## adjustr

### **Name**

**adjustr**(3) - \[CHARACTER:WHITESPACE\] Right-justify a string

### **Synopsis**
```fortran
    result = adjustr(string)
```
```fortran
     elemental character(len=len(string),kind=KIND) function adjustr(string)

      character(len=*,kind=KIND),intent(in) :: string
```
### **Characteristics**

- **string** is a _character_ variable
- The return value is a _character_ variable of the same kind and
  length as **string**

### **Description**

**adjustr**(3) right-justifies a string by removing trailing spaces. Spaces
are inserted at the start of the string as needed to retain the original
length.

### **Options**

- **string**
  : the string to right-justify

### **Result**

Trailing spaces are removed and the same number of spaces are inserted
at the start of **string**.

### **Examples**

Sample program:

```fortran
program demo_adjustr
implicit none
character(len=20) :: str
   ! print a short number line
   write(*,'(a)')repeat('1234567890',2)

  ! basic usage
   str = '  sample string '
   write(*,'(a)') str
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')repeat('1234567890',5)
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])
   write(*,'(a)')repeat('1234567890',5)

end program demo_adjustr
```
Results:
```text
   12345678901234567890
     sample string
          sample string
   12345678901234567890123456789012345678901234567890
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```
### **Standard**

Fortran 95

### **See Also**

[**adjustl**(3)](#adjustl),
[**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## aimag

### **Name**

**aimag**(3) - \[TYPE:NUMERIC\] Imaginary part of complex number

### **Synopsis**
```fortran
    result = aimag(z)
```
```fortran
     elemental complex(kind=KIND) function aimag(z)

      complex(kind=KIND),intent(in) :: z
```
### **Characteristics**

- The type of the argument **z** shall be _complex_ and any supported
  _complex_ kind

- The return value is of type _real_ with the kind type parameter of
  the argument.

### **Description**

  **aimag**(3) yields the imaginary part of the complex argument **z**.

  This is similar to the modern complex-part-designator **%IM** which also
  designates the imaginary part of a value, accept a designator can appear
  on the left-hand side of an assignment as well, as in **val%im=10.0**.

### **Options**

- **z**
  : The _complex_ value to extract the imaginary component of.

### **Result**

  The return value is a _real_ value with the magnitude and sign of the
  imaginary component of the argument **z**.

  That is, If **z** has the value **(x,y)**, the result has the value
  **y**.

### **Examples**

Sample program:

```fortran
program demo_aimag
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
character(len=*),parameter :: g='(*(1x,g0))'
complex              :: z4
complex(kind=real64) :: z8
   ! basics
    z4 = cmplx(1.e0, 2.e0)
    print *, 'value=',z4
    print g, 'imaginary part=',aimag(z4),'or', z4%im

    ! other kinds other than the default may be supported
    z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
    print *, 'value=',z8
    print g, 'imaginary part=',aimag(z8),'or', z8%im

    ! an elemental function can be passed an array
    print *
    print *, [z4,z4/2.0,z4+z4,z4**3]
    print *
    print *, aimag([z4,z4/2.0,z4+z4,z4**3])

end program demo_aimag
```
Results:
```text
 value= (1.00000000,2.00000000)
 imaginary part= 2.00000000 or 2.00000000
 value= (3.0000000000000000,4.0000000000000000)
 imaginary part= 4.0000000000000000 or 4.0000000000000000

 (1.00000000,2.00000000) (0.500000000,1.00000000) (2.00000000,4.00000000)
 (-11.0000000,-2.00000000)

   2.00000000       1.00000000       4.00000000      -2.00000000
```
### **Standard**

FORTRAN 77

### **See Also**

- [**cmplx**(3)](#cmplx) - Complex conversion function
- [**conjg**(3)](#conjg) - Complex conjugate function
- [**real**(3)](#real) - Convert to real type

Fortran has strong support for _complex_ values, including many intrinsics
that take or produce _complex_ values in addition to algebraic and
logical expressions:

[**abs**(3)](#abs),
[**acosh**(3)](#acosh),
[**acos**(3)](#acos),
[**asinh**(3)](#asinh),
[**asin**(3)](#asin),
[**atan2**(3)](#atan2),
[**atanh**(3)](#atanh),
[**atan**(3)](#atan),
[**cosh**(3)](#cosh),
[**cos**(3)](#cos),
[**co_sum**(3)](#co_sum),
[**dble**(3)](#dble),
[**dot_product**(3)](#dot_product),
[**exp**(3)](#exp),
[**int**(3)](#int),
[**is_contiguous**(3)](#is_contiguous),
[**kind**(3)](#kind),
[**log**(3)](#log),
[**matmul**(3)](#matmul),
[**precision**(3)](#precision),
[**product**(3)](#product),
[**range**(3)](#range),
[**rank**(3)](#rank),
[**sinh**(3)](#sinh),
[**sin**(3)](#sin),
[**sqrt**(3)](#sqrt),
[**storage_size**(3)](#storage_size),
[**sum**(3)](#sum),
[**tanh**(3)](#tanh),
[**tan**(3)](#tan),
[**unpack**(3)](#unpack),

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## aint

### **Name**

**aint**(3) - \[NUMERIC\] Truncate toward zero to  a whole number

### **Synopsis**
```fortran
    result = aint(x [,kind])
```
```fortran
     elemental real(kind=KIND) function iaint(x,kind)

      real(kind=**),intent(in)   :: x
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- the result is a real of the default kind unless **kind** is specified.
- **kind** is an _integer_ initialization expression indicating the
  kind parameter of the result.

### **Description**

  **aint**(3) truncates its argument toward zero to a whole number.

### **Options**

- **x**
  : the _real_ value to truncate.

- **kind**
  : indicates the kind parameter of the result.

### **Result**

  The sign is the same as the sign of **x** unless the magnitude of **x**
  is less than one, in which case zero is returned.

  Otherwise **aint**(3) returns the largest whole number that does not
  exceed the magnitude of **x** with the same sign as the input.

  That is, it truncates the value towards zero.

### **Examples**

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64
implicit none
real(kind=dp) :: x8
   print *,'basics:'
   print *,' just chops off the fractional part'
   print *,  aint(-2.999), aint(-2.1111)
   print *,' if |x| < 1 a positive zero is returned'
   print *,  aint(-0.999), aint( 0.9999)
   print *,' input may be of any real kind'
   x8 = 4.3210_dp
   print *, aint(-x8), aint(x8)
   print *,'elemental:'
   print *,aint([ &
    &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0,   &
    &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
end program demo_aint
```
Results:
```text
 basics:
  just chops off the fractional part
  -2.000000      -2.000000
  if |x| < 1 a positive zero is returned
  0.0000000E+00  0.0000000E+00
  input may be of any real kind
  -4.00000000000000        4.00000000000000
 elemental:
  -2.000000      -2.000000      -2.000000      -2.000000      -1.000000
  -1.000000      0.0000000E+00  0.0000000E+00  0.0000000E+00   1.000000
   1.000000       2.000000       2.000000       2.000000       2.000000
```
### **Standard**

FORTRAN 77

### **See Also**

[**anint**(3)](#anint),
[**int**(3)](#int),
[**nint**(3)](#nint),
[**selected_int_kind**(3)](#selected_int_kind),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## all

### **Name**

**all**(3) - \[ARRAY REDUCTION\] Determines if all the values are true

### **Synopsis**
```fortran
   result = all(mask [,dim])
```
```fortran
     logical function all(mask ,dim)

      logical,intent(in)          :: mask(..)
      integer,intent(in),optional :: dim
```
### **Characteristics**

### **Description**

**all**(3) determines if all the values are true in **mask**
in the array along dimension **dim**.

This is called a logical conjunction of elements of **mask** along
dimension **dim**.

The mask is generally a _logical_ expression, allowing for comparing
arrays and many other common operations.

### **Options**

- **mask**
  : shall be a logical array.

- **dim**
  : (optional) **dim** shall be a scalar integer with a value that lies
  between one and the rank of **mask**. The corresponding actual argument
  shall not be an optional dummy argument.

### **Result**

"**all(mask)**" returns a scalar value of type _logical_ where the kind
type parameter is the same as the kind type parameter of **mask**. If
**dim** is present, then **all(mask, dim)** returns an array with the rank
of **mask** minus 1. The shape is determined from the shape of **mask**
where the **dim** dimension is elided.

1.  **all(mask)** is true if all elements of **mask** are true. It also is
    true if **mask** has zero size; otherwise, it is false.

2.  If the rank of **mask** is one, then **all(mask, dim)** is equivalent
    to **all(mask)**. If the rank is greater than one, then **all(mask,
    dim)** is determined by applying **all()** to the array sections.

3.  Result Characteristics. The result is of type _logical_ with the
    same kind type parameter as **mask**. It is scalar if **dim**
    is absent or **n = 1**; otherwise, the result has rank **n - 1**
    and shape **\[d1, d2, . . ., dDIM-1, dDIM+1, . . ., dn \]**
    where **\[d1, d2, . . ., dn \]** is the shape of **mask**.

4.  Result Value.

    Case (i):
    : The result of **all(mask)** has the value true if all
    elements of **mask** are true or if **mask** has
    size zero, and the result has value false if any element
    of **mask** is false.

    Case (ii):
    : If **mask** has rank one, **all(mask,dim)** is equal to
    **all(mask)**. Otherwise, the value of element **(s1, s2,
    . . ., sdim-1, sdim+1, . . ., sn )** of all **(mask,
    dim)** is equal to **all(mask (s1, s2, . . ., sdim-1,
    :, sdim+1, . . ., sn ))**.

### **Examples**

Sample program:

```fortran
program demo_all
implicit none
logical bool
  ! basic usage
   ! is everything true?
   bool = all([ .true., .true., .true. ])
   bool = all([ .true., .false., .true. ])
   print *, bool
  ! by a dimension
   ARRAYS: block
   integer :: a(2,3), b(2,3)
    ! set everything to one except one value in b
    a = 1
    b = 1
    b(2,2) = 2
    ! now compare those two arrays
    print *,'entire array :', all(a .eq. b )
    print *,'compare columns:', all(a .eq. b, dim=1)
    print *,'compare rows:', all(a .eq. b, dim=2)
  end block ARRAYS
end program demo_all
```
Results:
```text
    T
    F
    entire array : F
    compare columns: T F T
    compare rows: T F
```
### **Standard**

Fortran 95

### **See Also**

[**any**(3)](#any)

 _fortran-lang intrinsic descriptions_

## allocated

### **Name**

**allocated**(3) - \[ARRAY INQUIRY\] Status of an allocatable entity

### **Synopsis**
```fortran
    result = allocated(entity)
```
```fortran
     logical function allocated(entity)

      type(TYPE(kind=KIND)),allocatable :: entity(..)
```
### **Characteristics**

  where **entity** may be any allocatable scalar or array object
  of any type.

### **Description**

  **allocated**(3) checks the allocation status of both arrays
  and scalars.

### **Options**

- **entity**
  : the _allocatable_ object to test.

### **Result**

  If the argument is allocated then the result is _.true._; otherwise,
  it returns _.false._.

### **Examples**

Sample program:

```fortran
program demo_allocated
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp), allocatable :: x(:)
character(len=256) :: message
integer :: istat
  ! basics
   if( allocated(x)) then
       write(*,*)'do things if allocated'
   else
       write(*,*)'do things if not allocated'
   endif

   ! if already allocated, deallocate
   if ( allocated(x) ) deallocate(x,STAT=istat, ERRMSG=message )
   if(istat.ne.0)then
      write(*,*)trim(message)
      stop
   endif

   ! only if not allocated, allocate
   if ( .not. allocated(x) ) allocate(x(20))

  ! allocation and intent(out)
   call intentout(x)
   write(*,*)'note it is deallocated!',allocated(x)

   contains

   subroutine intentout(arr)
   ! note that if arr has intent(out) and is allocatable,
   ! arr is deallocated on entry
   real(kind=sp),intent(out),allocatable :: arr(:)
       write(*,*)'note it was allocated in calling program',allocated(arr)
   end subroutine intentout

end program demo_allocated
```
Results:
```text
    T           4
    do things if allocated
    note it was allocated in calling program F
    note it is deallocated! F
```
### **Standard**

  Fortran 95. allocatable scalar entities were added in Fortran 2003.

### **See Also**

[**move_alloc**(3)](#move_alloc)

 _fortran-lang intrinsic descriptions_

## anint

### **Name**

**anint**(3) - \[NUMERIC\] Nearest whole number

### **Synopsis**
```fortran
    result = anint(a [,kind])
```
```fortran
     elemental real(kind=KIND) function iaint(x,kind)

      real(kind=KIND),intent(in)   :: x
      integer,intent(in),optional :: kind
```
- the _kind_ of the result is the same as **x** unless
  **kind** is present.

### **Characteristics**

### **Description**

**anint**(3) rounds its argument to the nearest whole number.

### **Options**

- **a**
  : the type of the argument shall be _real_.

- **kind**
  : (optional) an _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type real with the kind type parameter of the
argument if the optional **kind** is absent; otherwise, the kind type
parameter will be given by **kind**. If **a** is greater than zero,
**anint(a)**(3) returns **aint(a + 0.5)**. If **a** is less than or equal
to zero then it returns **aint(a - 0.5)**.

### **Examples**

Sample program:

```fortran
program demo_anint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 1.234E0_real32
   x8 = 4.321_real64
   print *, anint(x4), dnint(x8)
   x8 = anint(x4,kind=real64)
   print *, x8
   print *
   ! elemental
   print *,anint([ &
    & -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0, &
    & +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_anint
```
Results:

```text
    1.00000000       4.0000000000000000
    1.0000000000000000

   -3.00000000      -3.00000000      -2.00000000      -2.00000000
   -2.00000000      -1.00000000      -1.00000000       0.00000000
    1.00000000       1.00000000       2.00000000       2.00000000
    2.00000000       3.00000000       3.00000000
```
### **Standard**

FORTRAN 77

### **See Also**

[**aint**(3)](#aint),
[**int**(3)](#int),
[**nint**(3)](#nint),
[**selected_int_kind**(3)](#selected_int_kind),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _fortran-lang intrinsic descriptions_

### **Name**

**any**(3) - \[ARRAY REDUCTION\] Determines if any of the values in the logical array are _.true._

### **Synopsis**
```fortran
    result = any(mask [,dim])
```
```fortran
     logical(kind=KIND) function any(mask, dim)

      logical(kind=KIND),intent(in) :: mask(..)
      integer,intent(in),optional   :: dim
```
### **Characteristics**
- **mask** is a _logical_ array
- **dim** is a scalar integer
- the result is a logical array if **dim** is supplied,
  otherwise it is a logical scalar.

### **Description**

**any**(3) determines if any of the values in the logical
array **mask** along dimension **dim** are _.true._.

### **Options**

- **mask**
  : an array of _logical_ expressions or values to be tested in groups
  or in total for a _.true._ value.

- **dim**
  : a whole number value that lies between one and **rank(mask)** that
  indicates to return an array of values along the indicated dimension
  instead of a scalar answer.

### **Result**

**any(mask)** returns a scalar value of type _logical_ where the kind type
parameter is the same as the kind type parameter of **mask**. If **dim**
is present, then **any(mask, dim)** returns an array with the rank of
**mask** minus 1. The shape is determined from the shape of **mask**
where the **dim** dimension is elided.

1.  **any(mask)** is _.true._ if any element of **mask** is _.true._;
    otherwise, it is _.false._. It also is _.false._ if **mask** has
    zero size.

2.  If the rank of **mask** is one, then **any(mask, dim)** is
    equivalent to **any(mask)**. If the rank is greater than one, then
    **any(mask, dim)** is determined by applying **any(mask)** to the
    array sections.

### **Examples**

Sample program:

```fortran
program demo_any
implicit none
logical,parameter :: T=.true., F=.false.
integer           :: a(2,3), b(2,3)
logical           :: bool
  ! basic usage
   bool = any([F,F,T,F])
   print *,bool
   bool = any([F,F,F,F])
   print *,bool
  ! fill two integer arrays with values for testing
   a = 1
   b = 1
   b(:,2) = 2
   b(:,3) = 3
  ! using any(3) with logical expressions you can compare two arrays
  ! in a myriad of ways
   ! first, print where elements of b are bigger than in a
   call printl( 'first print b > a             ', b > a         )
   ! now use any() to test
   call printl( 'any true values?  any(b > a)  ', any(b > a )   )
   call printl( 'again by columns? any(b > a,1)', any(b > a, 1) )
   call printl( 'again by rows?    any(b > a,2)', any(b > a, 2) )
contains
! CONVENIENCE ROUTINE. this is not specific to ANY()
subroutine printl(title,a)
use, intrinsic :: iso_fortran_env, only : &
 & stderr=>ERROR_UNIT,&
 & stdin=>INPUT_UNIT,&
 & stdout=>OUTPUT_UNIT
implicit none

!@(#) print small 2d logical scalar, vector, or matrix

character(len=*),parameter   :: all='(*(g0,1x))'
character(len=*),parameter   :: row='(" > [ ",*(l1:,","))'
character(len=*),intent(in)  :: title
logical,intent(in)           :: a(..)
integer                      :: i
   write(*,*)
   write(*,all,advance='no')trim(title),&
    & ' : shape=',shape(a),',rank=',rank(a),',size=',size(a)
   ! get size and shape of input
   select rank(a)
   rank (0); write(*,'(a)')'(a scalar)'
      write(*,fmt=row,advance='no')a
      write(*,'(" ]")')
   rank (1); write(*,'(a)')'(a vector)'
      do i=1,size(a)
         write(*,fmt=row,advance='no')a(i)
         write(*,'(" ]")')
      enddo
   rank (2); write(*,'(a)')'(a matrix) '
      do i=1,size(a,dim=1)
         write(*,fmt=row,advance='no')a(i,:)
         write(*,'(" ]")')
      enddo
   rank default
      write(stderr,*)'*printl* did not expect rank=', rank(a), &
       & 'shape=', shape(a),'size=',size(a)
      stop '*printl* unexpected rank'
   end select

end subroutine printl

end program demo_any
```
Results:
```text
    T
    F

   first print b > a : shape=23,rank=2,size=6(a matrix)
    > [ F,T,T ]
    > [ F,T,T ]

   any true values?  any(b > a) : shape=,rank=0,size=1(a scalar)
    > [ T ]

   again by columns? any(b > a,1) : shape=3,rank=1,size=3(a vector)
    > [ F ]
    > [ T ]
    > [ T ]

   again by rows?    any(b > a,2) : shape=2,rank=1,size=2(a vector)
    > [ T ]
    > [ T ]
```
### **Standard**

Fortran 95

### **See Also**

[**all**(3)](#all)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## asinh

### **Name**

**asinh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function

### **Synopsis**
```fortran
    result = asinh(x)
```
```fortran
     elemental TYPE(kind=KIND) function asinh(x)

      TYPE(kind=KIND) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_
 - **KIND** may be any kind supported by the associated type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

**asinh**(3) computes the inverse hyperbolic sine of **x**.

### **Options**

- **x**
  : The value to compute the inverse hyperbolic sine of

### **Result**

The return value is of the same type and kind as **x**. If **x** is
_complex_, the imaginary part of the result is in radians and lies
between **-PI/2 \<= aimag(asinh(x)) \<= PI/2**.

### **Examples**

Sample program:
```fortran
program demo_asinh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]

    write (*,*) asinh(x)

end program demo_asinh
```
Results:
```text
  -0.88137358701954305  0.0000000000000000  0.88137358701954305
```
### **Standard**

Fortran 2008

### **See Also**

Inverse function: [**sinh**(3)](#sinh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_

## asin

### **Name**

**asin**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arcsine function

### **Synopsis**
```fortran
    result = asin(x)
```
```fortran
     elemental TYPE(kind=KIND) function asin(x)

      TYPE(kind=KIND) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_
 - **KIND** may be any kind supported by the associated type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

**asin**(3) computes the arcsine of its argument **x**.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

### **Options**

- **x**
  : The value to compute the arcsine of
  : The type shall be either _real_ and a magnitude that is less than or
  equal to one; or be _complex_.

### **Result**

- **result**
  The result has a value equal to a processor-dependent approximation
  to arcsin(x).

  If **x** is real the result is _real_ and it is expressed in radians
  and lies in the range
```fortran
        PI/2 <= ASIN (X) <= PI/2.
```
  If the argument (and therefore the result) is imaginary The real part
  of the result is in radians and lies in the range
```fortran
    -PI/2 <= real(asin(x)) <= PI/2
```
### **Examples**

The arcsine will allow you to find the measure of a right angle when you
know the ratio of the side opposite the angle to the hypotenuse.

So if you knew that a train track rose 1.25 vertical miles on a track
that was 50 miles long, you could determine the average angle of incline
of the track using the arcsine. Given

     sin(theta) = 1.25 miles/50 miles (opposite/hypotenuse)

Sample program:
```fortran
program demo_asin
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
! value to convert degrees to radians
real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp
real(kind=dp)           :: angle, rise, run
character(len=*),parameter :: all='(*(g0,1x))'
  ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
  ! then taking the arcsine of both sides of the equality yields
  ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
  rise=1.250_dp
  run=50.00_dp
  angle = asin(rise/run)
  print all, 'angle of incline(radians) = ', angle
  angle = angle/D2R
  print all, 'angle of incline(degrees) = ', angle

  print all, 'percent grade=',rise/run*100.0_dp
end program demo_asin
```
Results:
```
    angle of incline(radians) =    2.5002604899361139E-002
    angle of incline(degrees) =    1.4325437375665075
    percent grade=   2.5000000000000000
```
The percentage grade is the slope, written as a percent. To calculate
the slope you divide the rise by the run. In the example the rise is
1.25 mile over a run of 50 miles so the slope is 1.25/50 = 0.025.
Written as a percent this is 2.5 %.

For the US, two and 1/2 percent is generally thought of as the upper
limit. This means a rise of 2.5 feet when going 100 feet forward. In
the US this was the maximum grade on the first major US railroad, the
Baltimore and Ohio. Note curves increase the frictional drag on a
train reducing the allowable grade.

### **Standard**

FORTRAN 77 , for a _complex_ argument Fortran 2008

### **See Also**

Inverse function: [**sin**(3)](#sin)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## associated

### **Name**

**associated**(3) - \[STATE\] Status of a pointer or pointer/target pair

### **Synopsis**
```fortran
    result = associated(pointer [,target])
```
```fortran
     logical function associated(pointer,target)

      type(TYPE(kind=KIND),pointer :: pointer
      type(TYPE(kind=KIND),target,optional :: target
```
or
```fortran
     logical function associated(pointer,target)

      type(TYPE(kind=KIND),pointer :: pointer
      type(TYPE(kind=KIND),pointer,optional :: target
```
### **Characteristics**

  **pointer** shall have the _pointer_ attribute and it can be any type.

  **target** shall be a pointer or a target. It must have the
  same type, kind type parameter, and array rank as **pointer**.

  The association status of neither **pointer** nor **target** shall
  be undefined.

### **Description**

**associated**(3) determines the status of the pointer **pointer**
or if **pointer** is associated with the target **target**.

### **Options**

- **pointer**
  : A pointer to test for association

- **target**
  : A target that is to be tested for occupying the same storage
  units as the pointer **pointer**. That is, it is tested as to whether it
  is pointed to by **pointer**.

### **Result**

****associated**(3f) returns a scalar value of type _logical_.
There are several cases:

1.  When the optional **target** is not present then **associated(pointer)**
    is _.true._ if **pointer** is associated with a target; otherwise, it
    returns _.false._.

2.  If **target** is present and a scalar target, the result is _.true._ if
    **target** is not a zero-sized storage sequence and the target
    associated with **pointer** occupies the same storage units. If **pointer**
    is disassociated, the result is _.false._.

3.  If **target** is present and an array target, the result is _.true._ if
    **target** and **pointer** have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    **target** and **pointer** occupy the same storage units in array element
    order.

    As in case 2, the result is _.false._, if **pointer** is disassociated.

4.  If **target** is present and an scalar pointer, the result is _.true._ if
    **target** is associated with **pointer**, the target associated with **target**
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is _.false._, if either **target** or **pointer** is disassociated.

5.  If **target** is present and an array pointer, the result is _.true._ if
    target associated with **pointer** and the target associated with **target**
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and **target** and
    **pointer** occupy the same storage units in array element order. The
    result is _.false._, if either **target** or **pointer** is disassociated.

### **Examples**

Sample program:
```fortran
program demo_associated
implicit none
real, target  :: tgt(2) = [1., 2.]
real, pointer :: ptr(:)
   ptr => tgt
   if (associated(ptr)     .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED'
   if (associated(ptr,tgt) .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED TO TARGET'
end program demo_associated
```
### **Standard**

Fortran 95

### **See Also**

[**null**(3)](#null)

 _fortran-lang intrinsic descriptions_

## atan2

### **Name**

**atan2**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent (inverse tangent)
function

### **Synopsis**
```fortran
    result = atan2(y, x)
```
```fortran
     elemental function atan2(y, x)

      real,kind=KIND) :: atan2
      real,kind=KIND),intent(in) :: y, x
```
### **Characteristics**

**x** and **y** must be reals of the same kind.

The return value has the same type and kind as **y** and **x**.

### **Description**

**atan2**(3) computes in radians a processor-dependent approximation of
the arctangent of the complex number ( **x**, **y** ) or equivalently the
principal value of the arctangent of the value **y/x** (which determines
a unique angle).

### **Options**

- **y**
  : The imaginary component of the complex value **(x,y)** or the **y**
  component of the point **\<x,y\>**.

- **x**
  : The real component of the complex value **(x,y)** or the **x**
  component of the point **\<x,y\>**.

### **Result**

The value returned is by definition the principal value of the complex
number **(x, y)**.

The principal value is simply what we get when we adjust a radian value
to lie between **-PI** and **PI** inclusive,

The classic definition of the arctangent is the angle that is formed
in Cartesian coordinates of the line from the origin point **\<0,0\>**
to the point **\<x,y\>** .

Pictured as a vector it is easy to see that if **x** and **y** are both
zero the angle is indeterminate because it sits directly over the origin,
so **atan(0.0,0.0)** will produce an error.

Range of returned values by quadrant:
```text
>                   +PI/2
>                     |
>                     |
>     PI/2 < z < PI   |   0 > z < PI/2
>                     |
>   +-PI -------------+---------------- +-0
>                     |
>     PI/2 < -z < PI  |   0 < -z < PI/2
>                     |
>                     |
>                   -PI/2
>
     NOTES:

     If the processor distinguishes -0 and +0 then the sign of the
     returned value is that of Y when Y is zero, else when Y is zero
     the returned value is always positive.
```
### **Examples**

Sample program:
```fortran
program demo_atan2
real :: z
complex :: c

 ! basic usage
  ! ATAN2 (1.5574077, 1.0) has the value 1.0 (approximately).
  z=atan2(1.5574077, 1.0)
  write(*,*) 'radians=',z,'degrees=',r2d(z)

 ! elemental arrays
  write(*,*)'elemental',atan2( [10.0, 20.0], [30.0,40.0] )

 ! elemental arrays and scalars
  write(*,*)'elemental',atan2( [10.0, 20.0], 50.0 )

 ! break complex values into real and imaginary components
 ! (note TAN2() can take a complex type value )
  c=(0.0,1.0)
  write(*,*)'complex',c,atan2( x=c%re, y=c%im )

 ! extended sample converting cartesian coordinates to polar
  COMPLEX_VALS: block
  real                :: ang, radius
  complex,allocatable :: vals(:)

  vals=[ &
    ( 1.0, 0.0 ), & ! 0
    ( 1.0, 1.0 ), & ! 45
    ( 0.0, 1.0 ), & ! 90
    (-1.0, 1.0 ), & ! 135
    (-1.0, 0.0 ), & ! 180
    (-1.0,-1.0 ), & ! 225
    ( 0.0,-1.0 )]   ! 270
  do i=1,size(vals)
     call cartesian_to_polar(vals(i)%re, vals(i)%im, radius,ang)
     write(*,101)vals(i),ang,r2d(ang),radius
  enddo
  101 format(             &
  & 'X= ',f5.2,           &
  & ' Y= ',f5.2,          &
  & ' ANGLE= ',g0,        &
  & T38,'DEGREES= ',g0.4, &
  & T54,'DISTANCE=',g0)
 endblock COMPLEX_VALS

contains

elemental real function r2d(radians)
! input radians to convert to degrees
doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
real,intent(in)           :: radians
   r2d=radians / DEGREE ! do the conversion
end function r2d

subroutine cartesian_to_polar(x,y,radius,inclination)
! return angle in radians in range 0 to 2*PI
implicit none
real,intent(in)  :: x,y
real,intent(out) :: radius,inclination
   radius=sqrt(x**2+y**2)
   if(radius.eq.0)then
      inclination=0.0
   else
      inclination=atan2(y,x)
      if(inclination < 0.0)inclination=inclination+2*atan2(0.0d0,-1.0d0)
   endif
end subroutine cartesian_to_polar

end program demo_atan2
```
Results:
```text
    radians=   1.000000     degrees=   57.29578
    elemental  0.3217506      0.4636476
    elemental  0.1973956      0.3805064
    complex (0.0000000E+00,1.000000)   1.570796
   X=  1.00 Y=  0.00 ANGLE= .000000     DEGREES= .000   DISTANCE=1.000000
   X=  1.00 Y=  1.00 ANGLE= .7853982    DEGREES= 45.00  DISTANCE=1.414214
   X=  0.00 Y=  1.00 ANGLE= 1.570796    DEGREES= 90.00  DISTANCE=1.000000
   X= -1.00 Y=  1.00 ANGLE= 2.356194    DEGREES= 135.0  DISTANCE=1.414214
   X= -1.00 Y=  0.00 ANGLE= 3.141593    DEGREES= 180.0  DISTANCE=1.000000
   X= -1.00 Y= -1.00 ANGLE= 3.926991    DEGREES= 225.0  DISTANCE=1.414214
   X=  0.00 Y= -1.00 ANGLE= 4.712389    DEGREES= 270.0  DISTANCE=1.000000
```
### **Standard**

FORTRAN 77

### **See Also**

- [**atan**(3)](#atan)

### **Resources**

- [arctan:wikipedia](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)
 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## atanh

### **Name**

**atanh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function

### **Synopsis**
```fortran
    result = atanh(x)
```
```fortran
     elemental TYPE(kind=KIND) function atanh(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_
 - **KIND** may be any kind supported by the associated type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

**atanh**(3) computes the inverse hyperbolic tangent of **x**.

### **Options**

- **x**
  : The type shall be _real_ or _complex_.

### **Result**

The return value has same type and kind as **x**. If **x** is _complex_, the
imaginary part of the result is in radians and lies between
```fortran
     **-PI/2 <= aimag(atanh(x)) <= PI/2**
```
### **Examples**

Sample program:
```fortran
program demo_atanh
implicit none
real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]

   write (*,*) atanh(x)

end program demo_atanh
```
Results:
```text
   -Infinity   0.00000000             Infinity
```
### **Standard**

Fortran 2008

### **See Also**

Inverse function: [**tanh**(3)](#tanh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_

## atan

### **Name**

**atan**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

### **Synopsis**
```fortran
    result = atan([y], x)
```
```fortran
     elemental TYPE(kind=KIND) function atan(y,x)

      TYPE(kind=KIND),intent(in) :: x
      TYPE(kind=KIND),intent(in),optional :: y
```
### **Characteristics**

If **y**
is present **x** and **y** must both be _real_. Otherwise, **x** may be
_complex_. **KIND** can be any kind supported by the associated type.

The returned value is of the same type and kind as **x**.

### **Description**

**atan**(3) computes the arctangent of **x**.

### **Options**

- **x**
  : The value to compute the arctangent of.
  if **y** is present, **x** shall be _real_.

- **y**
  : Shall be of the same type and kind as **x**. If **x** is zero, **y**
  must not be zero.

### **Result**

The returned value is of the same type and kind as **x**. If **y** is
present, the result is identical to **atan2(y,x)**. Otherwise, it is the
arc tangent of **x**, where the real part of the result is in radians
and lies in the range
**-PI/2 \<= atan(x) \<= PI/2**

### **Examples**

Sample program:

```fortran
program demo_atan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64),parameter :: &
 Deg_Per_Rad = 57.2957795130823208767981548_real64
real(kind=real64) :: x
    x=2.866_real64
    print all, atan(x)

    print all, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad
    print all, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad
    print all, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad
    print all, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad

end program demo_atan
```
Results:
```text
   1.235085437457879
   .7853981633974483 45.00000000000000
   2.356194490192345 135.0000000000000
   -.7853981633974483 -45.00000000000000
   -2.356194490192345 -135.0000000000000
```
### **Standard**

FORTRAN 77  for a complex argument; and for two
arguments Fortran 2008

### **See Also**

[**atan2**(3)](#atan2), [**tan**(3)](#tan)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## atomic_add

### **Name**

**atomic_add**(3) - \[ATOMIC\] Atomic ADD operation

### **Synopsis**
```fortran
    call atomic_add (atom, value [,stat] )
```
```fortran
     subroutine atomic_add(atom,value,stat)

      integer(atomic_int_kind)            :: atom[*]
      integer(atomic_int_kind),intent(in) :: value
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value** is a scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Description**

**atomic_add**(3) atomically adds the value of VAR to the
variable **atom**. When **stat** is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso_fortran_env's STAT_STOPPED_IMAGE and if the remote image has
failed, the value STAT_FAILED_IMAGE.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:
```fortran
program demo_atomic_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_add (atom[1], this_image())
end program demo_atomic_add
```
### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_fetch_add**(3)](#atomic_fetch),
[**atomic_and**(3)](#atomic_and),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor)
**iso_fortran_env**(3),

 _fortran-lang intrinsic descriptions_

## atomic_and

### **Name**

**atomic_and**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation

### **Synopsis**
```fortran
    call atomic_and(atom, value [,stat])
```
```fortran
     subroutine atomic_and(atom,value,stat)

      integer(atomic_int_kind)            :: atom[*]
      integer(atomic_int_kind),intent(in) :: value
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value** is a scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Description**

**atomic_and**(3) atomically defines **atom** with the bitwise
**and** between the values of **atom** and **value**. When **stat** is present and the
invocation was successful, it is assigned the value 0. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed **atom**, if the remote image has stopped, it is
assigned the value of iso_fortran_env's stat_stopped_image and if
the remote image has failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_and(atom[1], int(b'10100011101'))
end program demo_atomic_and
```

### **Standard**

TS 18508

### **See Also**

[**atomic_fetch_and**(3)](#atomic_fetch_and),
[**atomic_define**(3)](#atomic_define),
[**atomic_ref**(3)](#atomic_ref),
[**atomic_cas**(3)](#atomic_cas),
**iso_fortran_env**(3),
[**atomic_add**(3)](#atomic_add),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor)

 _fortran-lang intrinsic descriptions_

## atomic_cas

### **Name**

**atomic_cas**(3) - \[ATOMIC\] Atomic compare and swap

### **Synopsis**
```fortran
    call atomic_cas (atom, old, compare, new [,stat] )
```
```fortran
     subroutine atomic_cas (atom, old, compare, new, stat)
```
### **Characteristics**

### **Description**

**atomic_cas**(3) compares the variable **atom** with the value of
**compare**; if the value is the same, **atom** is set to the value of
**new**. Additionally, **old** is set to the value of **atom** that was
used for the comparison. When **stat** is present and the invocation
was successful, it is assigned the value 0.  If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed **atom**, if the remote image has stopped, it is assigned
the value of iso_fortran_env's stat_stopped_image and if the remote
image has failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of either integer type with
  atomic_int_kind kind or logical type with atomic_logical_kind
  kind.

- **old**
  : Scalar of the same type and kind as **atom**.

- **compare**
  : Scalar variable of the same type and kind as **atom**.

- **new**
  : Scalar variable of the same type as **atom**. If kind is different, the
  value is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_cas
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*], prev
   call atomic_cas(atom[1], prev, .false., .true.)
end program demo_atomic_cas
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_ref**(3)](#atomic_ref),
[**iso_fortran_env**(3)](#)

 _fortran-lang intrinsic descriptions_

## atomic_define

### **Name**

**atomic_define**(3) - \[ATOMIC\] Setting a variable atomically

### **Synopsis**
```fortran
    call atomic_define (atom, value [,stat] )
```
```fortran
     subroutine atomic_define(atom, value, stat)

      TYPE(kind=atomic_KIND_kind) :: atom[*]
      TYPE(kind=KIND) :: value
      integer,intent(out),optional :: stat
```
### **Characteristics**

- **atom**
  : Scalar coarray or coindexed variable of either integer type with
  atomic_int_kind kind or logical type with atomic_logical_kind
  kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Description**

**atomic_define**(3) defines the variable **atom** with the value
**value** atomically.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable to atomically assign the
  value **value** to.
  kind.

- **value**
  : value to assign to **atom**

- **stat**
  : When **stat** is present and the invocation was
  successful, it is assigned the value **0**. If it is present and the
  invocation has failed, it is assigned a positive value; in particular,
  for a coindexed **atom**, if the remote image has stopped, it is assigned
  the value of iso_fortran_env's stat_stopped_image and if the remote
  image has failed, the value stat_failed_image.

### **Examples**

Sample program:

```fortran
program demo_atomic_define
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
    call atomic_define(atom[1], this_image())
end program demo_atomic_define
```

### **Standard**

Fortran 2008 ; with **stat**, TS 18508

### **See Also**

[**atomic_ref**(3)](#atomic_ref),
[**atomic_cas**(3)](#atomic_cas),
**iso_fortran_env**(3),
[**atomic_add**(3)](#atomic_add),
[**atomic_and**(3)](#atomic_and),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor)

 _fortran-lang intrinsic descriptions_

## atomic_fetch_add

### **Name**

**atomic_fetch_add**(3) - \[ATOMIC\] Atomic ADD operation with prior fetch

### **Synopsis**
```fortran
    call atomic_fetch_add(atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_add(atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_add**(3) atomically stores the value of **atom** in **old**
and adds the value of **var** to the variable **atom**. When **stat** is
present and the invocation was successful, it is assigned the value **0**.
If it is present and the invocation has failed, it is assigned a positive
value; in particular, for a coindexed **atom**, if the remote image has
stopped, it is assigned the value of iso_fortran_env's stat_stopped_image
and if the remote image has failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind. atomic_logical_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_fetch_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_add(atom[1], this_image(), old)
end program demo_atomic_fetch_add
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_add**(3)](#atomic_add),
**iso_fortran_env**(3),

[**atomic_fetch_and**(3)](#atomic_fetch_and),
[**atomic_fetch_or**(3)](#atomic_fetch_or),

[**atomic_fetch_xor**(3)](#atomic_fetch_xor)

 _fortran-lang intrinsic descriptions_

## atomic_fetch_and

### **Name**

**atomic_fetch_and**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation with prior fetch

### **Synopsis**
```fortran
    call atomic_fetch_and(atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_and(atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_and**(3) atomically stores the value of
**atom** in **old** and defines **atom** with the bitwise AND between the values of
**atom** and **value**. When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_fetch_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_and (atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_and
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_and**(3)](#atomic_and),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_fetch_add),
[**atomic_fetch_or**(3)](#atomic_fetch_or),

[**atomic_fetch_xor**(3)](#atomic_fetch_xor)

 _fortran-lang intrinsic descriptions_

## atomic_fetch_or

### **Name**

**atomic_fetch_or**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation with prior fetch

### **Synopsis**
```fortran
    call atomic_fetch_or(atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_or(atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_or**(3) atomically stores the value of
**atom** in **old** and defines **atom** with the bitwise OR between the values of
**atom** and **value**. When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_fetch_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_or(atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_or
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_or**(3)](#atomic_or),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_fetch_add),
[**atomic_fetch_and**(3)](#atomic_fetch_and),

[**atomic_fetch_xor**(3)](#atomic_fetch_xor)

 _fortran-lang intrinsic descriptions_

## atomic_fetch_xor

### **Name**

**atomic_fetch_xor**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise XOR operation with prior fetch

### **Synopsis**
```fortran
    call atomic_fetch_xor (atom, value, old [,stat] )
```
```fortran
     subroutine atomic_fetch_xor (atom, value, old, stat)
```
### **Characteristics**

### **Description**

**atomic_fetch_xor**(3) atomically stores the value of
**atom** in **old** and defines **atom** with the bitwise **xor** between the values of
**atom** and **value**. When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **old**
  : Scalar of the same type and kind as **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_fetch_xor
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_fetch_xor (atom[1], int(b'10100011101'), old)
end program demo_atomic_fetch_xor
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_xor**(3)](#atomic_xor),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_fetch_add),
[**atomic_fetch_and**(3)](#atomic_fetch_and),

[**atomic_fetch_or**(3)](#atomic_fetch_or)

 _fortran-lang intrinsic descriptions_

## atomic_or

### **Name**

**atomic_or**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation

### **Synopsis**
```fortran
    call atomic_or(atom, value [,stat] )
```
```fortran
     subroutine atomic_or(atom,value,stat)

      integer(atomic_int_kind)            :: atom[*]
      integer(atomic_int_kind),intent(in) :: value
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value** is a scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Description**

**atomic_or**(3) atomically defines **atom** with the bitwise **or**
between the values of **atom** and **value**. When **stat** is present and the
invocation was successful, it is assigned the value **0**. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed **atom**, if the remote image has stopped, it is
assigned the value of iso_fortran_env's stat_stopped_image and if
the remote image has failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_or
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_or(atom[1], int(b'10100011101'))
end program demo_atomic_or
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_fetch_or**(3)](#atomic_fetch),

[**iso_fortran_env**(3)](#),
[**atomic_add**(3)](#atomic_add),
[**atomic_or**(3)](#atomic_or),

[**atomic_xor**(3)](#atomic_xor)

 _fortran-lang intrinsic descriptions_

## atomic_ref

### **Name**

**atomic_ref**(3) - \[ATOMIC\] Obtaining the value of a variable atomically

### **Synopsis**
```fortran
    call atomic_ref(value, atom [,stat] )
```
```fortran
     subroutine atomic_ref(value,atom,stat)

      integer(atomic_int_kind),intent(in) :: value
      integer(atomic_int_kind)            :: atom[*]
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a ccalar coarray or coindexed variable of either integer
  type with atomic_int_kind kind or logical type with atomic_logical_kind
  kind.

- **value** is a scalar of the same type as **atom**. If the kind is
  different, the value is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Description**

**atomic_ref**(3) atomically assigns the value of the
variable **atom** to **value**. When **stat** is present and the invocation was
successful, it is assigned the value **0**. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed **atom**, if the remote image has stopped, it is assigned
the value of iso_fortran_env's **stat_stopped_image** and if the remote
image has failed, the value **stat_failed_image**.

### **Options**

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **atom**
  : Scalar coarray or coindexed variable of either integer type with
  atomic_int_kind kind or logical type with atomic_logical_kind
  kind.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

````fortran
program demo_atomic_ref
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*]
logical :: val
   call atomic_ref( val, atom[1] )
   ! ```
   call atomic_ref( val, atom[1] )
   if (val) then
      print *, "Obtained"
   endif
end program demo_atomic_ref
````

### **Standard**

Fortran 2008 ; with STAT, TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_cas**(3)](#atomic_cas),
[**iso_fortran_env**(3)](#),

[**atomic_fetch_add**(3)](#atomic_add),
[**atomic_fetch_and**(3)](#atomic_and),

[**atomic_fetch_or**(3)](#atomic_or),
[**atomic_fetch_xor**(3)](#atomic_xor)

 _fortran-lang intrinsic descriptions_

## atomic_xor

### **Name**

**atomic_xor**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation

### **Synopsis**
```fortran
    call atomic_xor(atom, value [,stat] )
```
```fortran
     subroutine atomic_xor(atom,value,stat)

      integer(atomic_int_kind)            :: atom[*]
      integer(atomic_int_kind),intent(in) :: value
      integer,intent(out),intent(out)     :: stat
```
### **Characteristics**

- **atom** is a scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value** is a scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat** is a Scalar default-kind integer variable.

### **Characteristics**

### **Description**

**atomic_xor**(3) atomically defines **atom** with the bitwise
**xor** between the values of **atom** and **value**. When **stat** is present and the
invocation was successful, it is assigned the value **0**. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed **atom**, if the remote image has stopped, it is
assigned the value of iso_fortran_env's stat_stopped_image and if
the remote image has failed, the value stat_failed_image.

### **Options**

- **atom**
  : Scalar coarray or coindexed variable of integer type with
  atomic_int_kind kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

### **Examples**

Sample program:

```fortran
program demo_atomic_xor
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_xor(atom[1], int(b'10100011101'))
end program demo_atomic_xor
```

### **Standard**

TS 18508

### **See Also**

[**atomic_define**(3)](#atomic_define),
[**atomic_fetch_xor**(3)](#atomic_fetch),
[**iso_fortran_env**(3)](#),
[**atomic_add**(3)](#atomic_add),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor)

 _fortran-lang intrinsic descriptions_

## bessel_j0

### **Name**

**bessel_j0**(3) - \[MATHEMATICS\] Bessel function of the first kind of order 0

### **Synopsis**
```fortran
    result = bessel_j0(x)
```
```fortran
     elemental real(kind=KIND) function bessel_j0(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - KIND may be any KIND supported by the _real_ type.
 - The result is the same type and kind as **x**.

### **Description**

**bessel_j0**(3) computes the Bessel function of the first kind
of order **0** of **x**.

### **Options**

- **x**
  : The value to operate on.

### **Result**

the Bessel function of the first kind of order **0** of **x**.
The result lies in the range **-0.4027 \<= bessel(0,x) \<= 1**.

### **Examples**

Sample program:

```fortran
program demo_bessel_j0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.0_real64
   x = bessel_j0(x)
   write(*,*)x
end program demo_bessel_j0
```
Results:

```text
      1.0000000000000000
```
### **Standard**

Fortran 2008

### **See Also**

[**bessel_j1**(3)](#bessel_j1),
[**bessel_jn**(3)](#bessel_jn),
[**bessel_y0**(3)](#bessel_y0),
[**bessel_y1**(3)](#bessel_y1),
[**bessel_yn**(3)](#bessel_yn)

 _fortran-lang intrinsic descriptions_

## bessel_j1

### **Name**

**bessel_j1**(3) - \[MATHEMATICS\] Bessel function of the first kind of order 1

### **Synopsis**
```fortran
    result = bessel_j1(x)
```
```fortran
     elemental real(kind=KIND) function bessel_j1(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 -  KIND may be any supported _real_ KIND.

### **Description**

**bessel_j1**(3) computes the Bessel function of the first kind
of order **1** of **x**.

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of type _real_ and lies in the range
**-0.5818 \<= bessel(0,x) \<= 0.5818** . It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_bessel_j1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
   x = bessel_j1(x)
   write(*,*)x
end program demo_bessel_j1
```

Results:

```text
     0.44005058574493350
```

### **Standard**

Fortran 2008

### **See Also**

[**bessel_j0**(3)](#bessel_j0),
[**bessel_jn**(3)](#bessel_jn),
[**bessel_y0**(3)](#bessel_y0),
[**bessel_y1**(3)](#bessel_y1),
[**bessel_yn**(3)](#bessel_yn)

 _fortran-lang intrinsic descriptions_

## bessel_jn

### **Name**

**bessel_jn**(3) - \[MATHEMATICS\] Bessel function of the first kind

### **Synopsis**
```fortran
    result = bessel_jn(n, x)
```
```fortran
     elemental real(kind=KIND) function bessel_jn(n,x)

      integer(kind=KIND),intent(in) :: n(..)
      real(kind=KIND),intent(in) :: x(..)
```
 - If **n** and **x** are arrays, their ranks and shapes
   shall conform.

 - The return value has the same type and kind as **x**.

```fortran
    result = bessel_jn(n1, n2, x)
```
```fortran
     real(kind=KIND) function bessel_jn(n1, n2, ,x)

     integer(kind=KIND),intent(in) :: n1(..)
     integer(kind=KIND),intent(in) :: n2(..)
     real(kind=KIND),intent(in) :: x
```
  The return value has the same type and kind as **x**.

### **Description**

  **bessel_jn( n, x )** computes the Bessel function of the first kind of
  order **n** of **x**.

  **bessel_jn(n1, n2, x)** returns an array with the Bessel
  function\|Bessel functions of the first kind of the orders **n1**
  to **n2**.

### **Options**

- **n**
  : Shall be a scalar or an array of type _integer_.

- **n1**
  : Shall be a non-negative scalar of type _integer_.

- **n2**
  : Shall be a non-negative scalar of type _integer_.

- **x**
  : Shall be a scalar or an array of type _real_.
  For **bessel_jn(n1, n2, x)** it shall be scalar.

### **Result**

The return value is a scalar of type _real_. It has the same kind
as **x**.

### **Examples**

Sample program:

```fortran
program demo_bessel_jn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = bessel_jn(5,x)
    write(*,*)x
end program demo_bessel_jn
```

Results:

```text
      2.4975773021123450E-004
```

### **Standard**

Fortran 2008

### **See Also**

[**bessel_j0**(3)](#bessel_j0),
[**bessel_j1**(3)](#bessel_j1),
[**bessel_y0**(3)](#bessel_y0),
[**bessel_y1**(3)](#bessel_y1),
[**bessel_yn**(3)](#bessel_yn)

 _fortran-lang intrinsic descriptions_

## bessel_y0

### **Name**

**bessel_y0**(3) - \[MATHEMATICS\] Bessel function of the second kind of order 0

### **Synopsis**
```fortran
    result = bessel_y0(x)
```
```fortran
     elemental real(kind=KIND) function bessel_y0(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - KIND may be any supported _real_ KIND.

### **Description**

**bessel_y0**(3) computes the Bessel function of the second
kind of order 0 of **x**.

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of type _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_bessel_y0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 0.0_real64
  x = bessel_y0(x)
  write(*,*)x
end program demo_bessel_y0
```

Results:

```text
                    -Infinity
```

### **Standard**

Fortran 2008

### **See Also**

[**bessel_j0**(3)](#bessel_j0),
[**bessel_j1**(3)](#bessel_j1),
[**bessel_jn**(3)](#bessel_jn),
[**bessel_y1**(3)](#bessel_y1),
[**bessel_yn**(3)](#bessel_yn)

 _fortran-lang intrinsic descriptions_

## bessel_y1

### **Name**

**bessel_y1**(3) - \[MATHEMATICS\] Bessel function of the second kind of order 1

### **Synopsis**
```fortran
    result = bessel_y1(x)
```
```fortran
     elemental real(kind=KIND) function bessel_y1(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - KIND may be any supported _real_ KIND.

### **Description**

**bessel_y1**(3) computes the Bessel function of the second
kind of order 1 of **x**.

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_bessel_y1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 1.0_real64
  write(*,*)x, bessel_y1(x)
end program demo_bessel_y1
```

### **Standard**

Fortran 2008

### **See Also**

[**bessel_j0**(3)](#bessel_j0),
[**bessel_j1**(3)](#bessel_j1),
[**bessel_jn**(3)](#bessel_jn),
[**bessel_y0**(3)](#bessel_y0),
[**bessel_yn**(3)](#bessel_yn)

 _fortran-lang intrinsic descriptions_

## bessel_yn

### **Name**

**bessel_yn**(3) - \[MATHEMATICS\] Bessel function of the second kind

### **Synopsis**
```fortran
    result = bessel_yn(n, x)
```
```fortran
     elemental real(kind=KIND) function bessel_yn(n,x)

      integer(kind=KIND),intent(in) :: n(..)
      real(kind=KIND),intent(in) :: x(..)
```
### **Characteristics**

 - If **n** and **x** are arrays, their ranks and shapes
   shall conform.

 - The return value has the same type and kind as **x**.

```fortran
    result = bessel_yn(n1, n2, x)
```
```fortran
     real(kind=KIND) function bessel_yn(n1, n2, ,x)

      integer(kind=KIND),intent(in) :: n1(..)
      integer(kind=KIND),intent(in) :: n2(..)
      real(kind=KIND),intent(in) :: x
```
  The return value has the same type and kind as **x**.

### **Description**

  **bessel_yn(n, x)** computes the Bessel function of the second kind
  of order **n** of **x**. If **n** and **x** are arrays, their ranks
  and shapes shall conform.

  **bessel_yn(n1, n2, x)** returns an array with the Bessel
  function\|Bessel functions of the first kind of the orders **n1**
  to **n2**.

### **Options**

- **n**
  : Shall be a scalar or an array of type _integer_.

- **n1**
  : Shall be a non-negative scalar of type _integer_.

- **n2**
  : Shall be a non-negative scalar of type _integer_.

- **x**
  : Shall be a scalar or an array of type _real_; for
  **bessel_yn(n1, n2, x)** it shall be scalar.

### **Result**

The return value is _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_bessel_yn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
  write(*,*) x,bessel_yn(5,x)
end program demo_bessel_yn
```

Results:

```text
      1.0000000000000000       -260.40586662581222
```

### **Standard**

Fortran 2008

### **See Also**

[**bessel_j0**(3)](#bessel_j0),
[**bessel_j1**(3)](#bessel_j1),
[**bessel_jn**(3)](#bessel_jn),
[**bessel_y0**(3)](#bessel_y0),
[**bessel_y1**(3)](#bessel_y1)

 _fortran-lang intrinsic descriptions_

## bge

### **Name**

**bge**(3) - \[BIT:COMPARE\] Bitwise greater than or equal to

### **Synopsis**
```fortran
    result = bge(i,j)
```
```fortran
      elemental logical function bge(i, j)

       integer(kind=**),intent(in) :: i
       integer(kind=**),intent(in) :: j
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

 - the _integer_ _kind_ of **i** and **j** may not necessarily be
   the same.  In addition, values may be a BOZ constant with a value
   valid for the _integer_ kind available with the most bits on the
   current platform.

 - The return value is of type _logical_ and of the default kind.

### **Description**

  **bge**(3) Determines whether one _integer_ is bitwise greater than
  or equal to another.

  The bit-level representation of a value is platform dependent. The
  endian-ness of a system and whether the system uses a "two's complement"
  representation of signs can affect the results, for example.

  A BOZ constant (Binary, Octal, Hexadecimal) does not have a _kind_
  or _type_ of its own, so be aware it is subject to truncation when
  transferred to an _integer_ type. The most bits the constant may
  contain is limited by the most bits representable by any _integer_
  kind supported by the compilation.

#### Bit Sequence Comparison

  When bit sequences of unequal length are compared, the shorter sequence
  is padded with zero bits on the left to the same length as the longer
  sequence (up to the largest number of bits any available _integer_ kind
  supports).

  Bit sequences are compared from left to right, one bit at a time,
  until unequal bits are found or until all bits have been compared and
  found to be equal.

  The bits are always evaluated in this order, not necessarily from MSB
  to LSB (most significant bit to least significant bit).

  If unequal bits are found the sequence with zero in the unequal
  position is considered to be less than the sequence with one in the
  unequal position.

### **Options**

- **i**
  : The value to test if >= **j** based on the bit representation
    of the values.

- **j**
  : The value to test **i** against.

### **Result**

  Returns _.true._ if **i** is bit-wise greater than **j** and _.false._
  otherwise.

### **Examples**

Sample program:
```fortran
program demo_bge
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer            :: i
integer(kind=int8) :: byte
integer(kind=int8),allocatable :: arr1(:), arr2(:)

  ! BASIC USAGE
   write(*,*)'bge(-127,127)=',bge( -127, 127 )
   ! on (very common) "two's complement" machines that are
   ! little-endian -127 will be greater than 127

   ! BOZ constants
   ! BOZ constants are subject to truncation, so make sure
   ! your values are valid for the integer kind being compared to
   write(*,*)'bge(b"0001",2)=',bge( b"1", 2)

  ! ELEMENTAL
   ! an array and scalar
   write(*, *)'compare array of values [-128, -0, +0, 127] to 127'
   write(*, *)bge(int([-128, -0, +0, 127], kind=int8), 127_int8)

   ! two arrays
   write(*, *)'compare two arrays'
   arr1=int( [ -127, -0, +0,  127], kind=int8 )
   arr2=int( [  127,  0,  0, -127], kind=int8 )
   write(*,*)'arr1=',arr1
   write(*,*)'arr2=',arr2
   write(*, *)'bge(arr1,arr2)=',bge( arr1, arr2 )

  ! SHOW TESTS AND BITS
   ! actually looking at the bit patterns should clarify what affect
   ! signs have ...
   write(*,*)'Compare some one-byte values to 64.'
   write(*,*)'Notice that the values are tested as bits not as integers'
   write(*,*)'so the results are as if values are unsigned integers.'
   do i=-128,127,32
      byte=i
      write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,bge(byte,64_int8),byte
   enddo

  ! SIGNED ZERO
   ! are +0 and -0 the same on your platform? When comparing at the
   ! bit level this is important
   write(*,'("plus zero=",b0)')  +0
   write(*,'("minus zero=",b0)') -0

end program demo_bge
```
Results:

  How an integer value is represented at the bit level can vary. These
  are just the values expected on Today's most common platforms ...

```text
    > bge(-127,127)= T
    > bge(b"0001",2)= F
    > compare array of values [-128, -0, +0, 127] to 127
    > T F F T
    > compare two arrays
    > arr1= -127    0    0  127
    > arr2=  127    0    0 -127
    > bge(arr1,arr2)= T T T F
    > Compare some one-byte values to 64.
    > Notice that the values are tested as bits not as integers
    > so the results are as if values are unsigned integers.
    > -0128  T 10000000
    > -0096  T 10100000
    > -0064  T 11000000
    > -0032  T 11100000
    > +0000  F 00000000
    > +0032  F 00100000
    > +0064  T 01000000
    > +0096  T 01100000
    > plus zero=0
    > minus zero=0
```
### **Standard**

Fortran 2008

### **See Also**

[**bgt**(3)](#bgt),
[**ble**(3)](#ble),
[**blt**(3)](#blt)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## bgt

### **Name**

**bgt**(3) - \[BIT:COMPARE\] Bitwise greater than

### **Synopsis**
```fortran
    result = bgt(i, j)
```
```fortran
      elemental logical function bgt(i, j)

       integer(kind=KIND),intent(in) :: i
       integer(kind=KIND),intent(in) :: j
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

 - the _integer_ _kind_ of **i** and **j** may not necessarily be the same.
   kind. In addition, values may be a BOZ constant with a value valid
   for the _integer_ kind available with the most bits on the current
   platform.

 - The return value is of type _logical_ and of the default kind.

### **Description**

  **bgt** determines whether an integer is bitwise greater than another.
  Bit-level representations of values are platform-dependent.

### **Options**

- **i**
  : Shall be of _integer_ type or a BOZ literal constant.

- **j**
  : Shall be of _integer_ type or a BOZ literal constant.

### **Result**

  The return value is of type _logical_ and of the default kind. The
  result is true if the sequence of bits represented by _i_ is greater
  than the sequence of bits represented by _j_, otherwise the result
  is false.

### **Examples**

Sample program:
```fortran
program demo_bgt
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer            :: i
integer(kind=int8) :: byte
  ! Compare some one-byte values to 64.
   ! Notice that the values are tested as bits not as integers
   ! so sign bits in the integer are treated just like any other
   do i=-128,127,32
      byte=i
      write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,bgt(byte,64_int8),byte
   enddo

   ! see the BGE() description for an extended description
   ! of related information

end program demo_bgt
```
Results:
```text
   > -0128  T 10000000
   > -0096  T 10100000
   > -0064  T 11000000
   > -0032  T 11100000
   > +0000  F 00000000
   > +0032  F 00100000
   > +0064  F 01000000
   > +0096  T 01100000
```
### **Standard**

Fortran 2008

### **See Also**

[**bge**(3)](#bge),
[**ble**(3)](#ble),
[**blt**(3)](#blt)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## bit_size

### **Name**

**bit_size**(3) - \[BIT:INQUIRY\] Bit size inquiry function

### **Synopsis**
```fortran
    result = bit_size(i)
```
```fortran
     integer(kind=KIND) function bit_size(i)

      integer(kind=KIND),intent(in) :: i(..)
```
### **Characteristics**

 - the value of **KIND** is any valid value for an _integer_ kind
   parameter on the processor.
 - the return value is of the same kind as the input value.

### **Description**

  **bit_size**(3) returns the number of bits (integer precision plus
  sign bit) represented by the type of the _integer_ **i**.

### **Options**

- **i**
  : An _integer_ value of any kind whose size in bits is to be determined.
  Because only the type of the argument is examined, the argument need not
  be defined; **i** can be a scalar or an array, but a scalar representing
  just a single element is always returned.

### **Result**

The number of bits used to represent a value of the type
of __i__. The result is a _integer_ scalar of the same kind as __i__.

### **Examples**

Sample program:

```fortran
program demo_bit_size
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : integer_kinds
implicit none
character(len=*),parameter   :: fmt=&
& '(a,": bit size is ",i3," which is kind=",i3," on this platform")'

    ! default integer bit size on this platform
    write(*,fmt) "default", bit_size(0), kind(0)

    write(*,fmt) "int8   ", bit_size(0_int8),   kind(0_int8)
    write(*,fmt) "int16  ", bit_size(0_int16),  kind(0_int16)
    write(*,fmt) "int32  ", bit_size(0_int32),  kind(0_int32)
    write(*,fmt) "int64  ", bit_size(0_int64),  kind(0_int64)

    write(*,'(a,*(i0:,", "))') "The available kinds are ",integer_kinds

end program demo_bit_size
```
Typical Results:
```text
    default: bit size is  32 which is kind=  4 on this platform
    int8   : bit size is   8 which is kind=  1 on this platform
    int16  : bit size is  16 which is kind=  2 on this platform
    int32  : bit size is  32 which is kind=  4 on this platform
    int64  : bit size is  64 which is kind=  8 on this platform
    The available kinds are 1, 2, 4, 8, 16
```
### **Standard**

Fortran 95

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

### **Name**

**ble**(3) - \[BIT:COMPARE\] Bitwise less than or equal to

### **Synopsis**
```fortran
    result = ble(i,j)
```
```fortran
     elemental logical function ble(i, j)

      integer(kind=KIND),intent(in) :: i
      integer(kind=KIND),intent(in) :: j
```
### **Characteristics**

  - the _kind_ of **i** and **j** may be of any supported _integer_
  kind, not necessarily the same. An exception is that values may be a
  BOZ constant with a value valid for the _integer_ kind available with
  the most bits on the current platform.
  - a logical scalar of default kind is returned.

### **Description**

  **ble**(3) determines whether an integer is bitwise less than or equal
  to another.

### **Options**

- **i**
  : the value to use for testing **j**

- **j**
  : the value to be tested for being less than or equal to **i**

### **Result**

The return value is _.true._ if any bit in **j** is less than any bit in
**i** starting with the rightmost bit and continuing tests leftward.

### **Examples**

Sample program:
```fortran
program demo_ble
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer            :: i
integer(kind=int8) :: byte
  ! Compare some one-byte values to 64.
   ! Notice that the values are tested as bits not as integers
   ! so sign bits in the integer are treated just like any other
   do i=-128,127,32
      byte=i
      write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,ble(byte,64_int8),byte
      write(*,'(sp,i0.4,*(4x,b0.8))')64_int8,64_int8
   enddo

   ! see the BGE() description for an extended description
   ! of related information

end program demo_ble
```
Results:
```text
   -0128  F 10000000
   +0064    01000000
   -0096  F 10100000
   +0064    01000000
   -0064  F 11000000
   +0064    01000000
   -0032  F 11100000
   +0064    01000000
   +0000  T 00000000
   +0064    01000000
   +0032  T 00100000
   +0064    01000000
   +0064  T 01000000
   +0064    01000000
   +0096  F 01100000
   +0064    01000000
```
### **Standard**

Fortran 2008

### **See Also**

[**bge**(3)](#bge),
[**bgt**(3)](#bgt),
[**blt**(3)](#blt)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## blt

### **Name**

**blt**(3) - \[BIT:COMPARE\] Bitwise less than

### **Synopsis**
```fortran
    result = blt(i,j)
```
```fortran
     elemental logical function blt(i, j)

      integer(kind=KIND),intent(in) :: i
      integer(kind=KIND),intent(in) :: j
```
### **Characteristics**

  where the _kind_ of **i** and **j** may be of any supported _integer_
  kind, not necessarily the same. An exception is that values may be a
  BOZ constant with a value valid for the _integer_ kind available with
  the most bits on the current platform.

### **Description**

  **blt**(3) determines whether an integer is bitwise less than another.

### **Options**

- **i**
  : Shall be of _integer_ type or a BOZ literal constant.

- **j**
  : Shall be of _integer_ type or a BOZ constant.

### **Result**

The return value is of type _logical_ and of the default kind.

### **Examples**

Sample program:
```fortran
program demo_blt
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer            :: i
integer(kind=int8) :: byte
  ! Compare some one-byte values to 64.
   ! Notice that the values are tested as bits not as integers
   ! so sign bits in the integer are treated just like any other
   do i=-128,127,32
      byte=i
      write(*,'(sp,i0.4,*(1x,1l,1x,b0.8))')i,blt(byte,64_int8),byte
   enddo

   ! see the BGE() description for an extended description
   ! of related information

end program demo_blt
```
Results:
```text
   > -0128  F 10000000
   > -0096  F 10100000
   > -0064  F 11000000
   > -0032  F 11100000
   > +0000  T 00000000
   > +0032  T 00100000
   > +0064  F 01000000
   > +0096  F 01100000
```
### **Standard**

Fortran 2008

### **See Also**

[**bge**(3)](#bge),
[**bgt**(3)](#bgt),
[**ble**(3)](#ble)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## btest

### **Name**

**btest**(3) - \[BIT:INQUIRY\] Tests a bit of an _integer_ value.

### **Synopsis**
```fortran
    result = btest(i,pos)
```
```fortran
     elemental integer(kind=KIND) function btest(i,pos)

      integer,intent(in)  :: i
      logical,intent(out) :: pos
```
### **Characteristics**

where **KIND** is any _integer_ kind supported by the programming environment.

### **Description**

**btest**(3) returns logical _.true._ if the bit at **pos** in **i** is set.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The bit position to query. it must be a valid position for the
  value **i**; ie. **0 <= pos <= bit_size(i)** .

  A value of zero refers to the least significant bit.

### **Result**

The result is a _logical_ that has the value _.true._ if bit
position **pos** of **i** has the value **1** and the value
_.false._ if bit **pos** of **i** has the value **0**.

### **Examples**

Sample program:

```fortran
program demo_btest
implicit none
integer :: i, j, pos, a(2,2)
logical :: bool
character(len=*),parameter :: g='(*(g0))'

     i = 32768 + 1024 + 64
    write(*,'(a,i0,"=>",b32.32,/)')'Looking at the integer: ',i

    ! looking one bit at a time from LOW BIT TO HIGH BIT
    write(*,g)'from bit 0 to bit ',bit_size(i),'==>'
    do pos=0,bit_size(i)-1
        bool = btest(i, pos)
        write(*,'(l1)',advance='no')bool
    enddo
    write(*,*)

    ! a binary format the hard way.
    ! Note going from bit_size(i) to zero.
    write(*,*)
    write(*,g)'so for ',i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')i
    write(*,g)merge('^','_',[(btest(i,j),j=bit_size(i)-1,0,-1)])
    write(*,*)
    write(*,g)'and for ',-i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')-i
    write(*,g)merge('^','_',[(btest(-i,j),j=bit_size(i)-1,0,-1)])

    ! elemental:
    !
    a(1,:)=[ 1, 2 ]
    a(2,:)=[ 3, 4 ]
    write(*,*)
    write(*,'(a,/,*(i2,1x,i2,/))')'given the array a ...',a
    ! the second bit of all the values in a
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (a, 2)',btest(a,2)
    ! bits 1,2,3,4 of the value 2
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (2, a)',btest(2,a)
end program demo_btest
```

Results:

```text
Looking at the integer: 33856=>11111111111111110111101111000000

00000000000000001000010001000000
11111111111111110111101111000000
1000010001000000
11111111111111110111101111000000
from bit 0 to bit 32==>
FFFFFFTFFFTFFFFTFFFFFFFFFFFFFFFF

so for 33856 with a bit size of 32
00000000000000001000010001000000
________________^____^___^______

and for -33856 with a bit size of 32
11111111111111110111101111000000
^^^^^^^^^^^^^^^^_^^^^_^^^^______

given the array a ...
 1  3
 2  4

the value of btest (a, 2)
 F  F
 F  T

the value of btest (2, a)
 T  F
 F  F
```

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## c_associated

### **Name**

**c_associated**(3) - \[ISO_C_BINDING\] Status of a C pointer

### **Synopsis**
```fortran
    result = c_associated(c_prt_1, [c_ptr_2] )
```
```fortran
     logical function c_associated(c_prt_1, cptr_2)

      TYPE,intent(in) ::c_ptr_1
      TYPE,intent(in),optional ::c_ptr_2
```
### **Characteristics**

- **c_ptr_1** is a scalar of the type c_ptr or c_funptr.
- **c_ptr_2** is a scalar of the same type as c_ptr_1.
- The return value is of type _logical_

### **Description**

**c_associated**(3) determines the status of the
C pointer c_ptr_1 or if c_ptr_1 is associated with the target
c_ptr_2.

### **Options**

- **c_ptr_1**
  : C pointer to test for being a C NULL pointer, or to test if
  pointing to the same association as **c_ptr_2** when present.

- **c_ptr_2**
  : C pointer to test for shared association with **c_ptr_1**

### **Result**

The return value is of type _logical_; it is _.false_. if either c_ptr_1
is a C NULL pointer or if c_ptr1 and c_ptr_2 point to different
addresses.

### **Examples**

Sample program:

```fortran
program demo_c_associated

contains

subroutine association_test(a,b)
use iso_c_binding, only: c_associated, c_loc, c_ptr
implicit none
real, pointer :: a
type(c_ptr) :: b
   if(c_associated(b, c_loc(a))) &
      stop 'b and a do not point to same target'
end subroutine association_test

end program demo_c_associated
```

### **Standard**

Fortran 2003

### **See Also**

[**c_loc**(3)](#c_loc),
[**c_funloc**(3)](#c_funloc),
**iso_c_binding**(3)

 _fortran-lang intrinsic descriptions_

## ceiling

### **Name**

**ceiling**(3) - \[NUMERIC\] Integer ceiling function

### **Synopsis**
```fortran
    result = ceiling(a [,kind])
```
```fortran
     elemental integer(KIND) function ceiling(a,kind)

      real(kind=KIND),intent(in)  :: a
      integer,intent(in),optional :: kind
```
### **Characteristics**

where the _kind_ of the result KIND is the same as **a** unless its
kind is specified by the optional **kind** argument.

### **Description**

**ceiling**(3) returns the least integer greater than or equal to **a**.

### **Options**

- **a**
  : A _real_ value to produce a result for.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The result will be the _integer_ value equal to **a** or the least integer
greater than **a** if the input value is not equal to a whole number.

On the number line -n <-- 0 -> +n the value returned is always at or to the
right of the input value.

The result is undefined if it cannot be represented in the specified
_integer_ type.

### **Examples**

Sample program:

```fortran
program demo_ceiling
implicit none
! just a convenient format for a list of integers
character(len=*),parameter :: ints='(*("   > ",5(i0:,",",1x),/))'
real :: x
real :: y
  ! basic usage
   x = 63.29
   y = -63.59
   print ints, ceiling(x)
   print ints, ceiling(y)
   ! note the result was the next integer larger to the right

  ! real values equal to whole numbers
   x = 63.0
   y = -63.0
   print ints, ceiling(x)
   print ints, ceiling(y)

  ! elemental (so an array argument is allowed)
   print ints , &
   & ceiling([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, &
   &  -1.0,  -0.5,  0.0, +0.5, +1.0, &
   &  +1.5,  +2.0, +2.2, +2.5, +2.7  ])

end program demo_ceiling
```
Results:
```text
   > 64
   > -63
   > 63
   > -63
   > -2, -2, -2, -2, -1,
   > -1, 0, 0, 1, 1,
   > 2, 2, 3, 3, 3
```
### **Standard**

Fortran 95

### **See Also**

[**floor**(3)](#floor),
[**nint**(3)](#nint)

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**int**(3)](#int),
[**selected_int_kind**(3)](#selected_int_kind)

 _fortran-lang intrinsic descriptions_

## c_f_pointer

### **Name**

**c_f_pointer**(3) - \[ISO_C_BINDING\] Convert C into Fortran pointer

### **Synopsis**
```fortran
    call c_f_pointer(cptr, fptr [,shape] )
```
```fortran
     subroutine c_f_pointer(cptr, fptr ,shape )

      type(c_ptr),intent(in) :: cprt
      type(TYPE),pointer,intent(out) :: fprt
      integer,intent(in),optional :: shape(:)
```
### **Characteristics**

The Fortran pointer **fprt** must be interoperable with **cptr**

**shape** is only specified if **fptr** is an array.

### **Description**

**c_f_pointer**(3) assigns the target (the C pointer **cptr**) to the
Fortran pointer **fptr** and specifies its shape if **fptr** points to
an array.

### **Options**

- **cptr**
  : scalar of the type c_ptr. It is **intent(in)**.

- **fptr**
  : pointer interoperable with **cptr**. it is **intent(out)**.

- **shape**
  : (Optional) Rank-one array of type _integer_ with **intent(in)** .
  It shall be present if and only if **fptr** is an array. The size
  must be equal to the rank of **fptr**.

### **Examples**

Sample program:

```fortran
program demo_c_f_pointer
use iso_c_binding
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
      import :: c_ptr
      type(c_ptr), intent(out) :: p
   end subroutine
end interface
type(c_ptr) :: cptr
real,pointer :: a(:)
   call my_routine(cptr)
   call c_f_pointer(cptr, a, [12])
end program demo_c_f_pointer
```

### **Standard**

Fortran 2003

### **See Also**

[**c_loc**(3)](#c_loc),
[**c_f_procpointer**(3)](#c_f_procpointer),
**iso_c_binding**(3)

 _fortran-lang intrinsic descriptions_

## c_f_procpointer

### **Name**

**c_f_procpointer**(3) - \[ISO_C_BINDING\] Convert C into Fortran procedure pointer

### **Synopsis**
```fortran
    call c_f_procpointer(cptr, fptr)
```
```fortran
     subroutine c_f_procpointer(cptr, fptr )

      type(c_funptr),intent(in) :: cprt
      type(TYPE),pointer,intent(out) :: fprt
```
### **Characteristics**

### **Description**

**c_f_procpointer**(3) assigns the target of the C function
pointer **cptr** to the Fortran procedure pointer **fptr**.

### **Options**

- **cptr**
  : scalar of the type c_funptr. It is **intent(in)**.

- **fptr**
  : procedure pointer interoperable with **cptr**. It is **intent(out)**.

### **Examples**

Sample program:

```fortran
program demo_c_f_procpointer
use iso_c_binding
implicit none
abstract interface
   function func(a)
   import :: c_float
   real(c_float), intent(in) :: a
   real(c_float) :: func
   end function
end interface
interface
   function getIterFunc() bind(c,name="getIterFunc")
   import :: c_funptr
   type(c_funptr) :: getIterFunc
   end function
end interface
type(c_funptr) :: cfunptr
procedure(func), pointer :: myFunc
   cfunptr = getIterFunc()
   call c_f_procpointer(cfunptr, myFunc)
end program demo_c_f_procpointer
```

### **Standard**

Fortran 2003

### **See Also**

[**c_loc**(3)](#c_loc),
[**c_f_pointer**(3)](#c_f_pointer),
**iso_c_binding**(3)

 _fortran-lang intrinsic descriptions_

## c_funloc

### **Name**

**c_funloc**(3) - \[ISO_C_BINDING\] Obtain the C address of a procedure

### **Synopsis**
```fortran
    result = c_funloc(x)
```
```fortran
```
### **Characteristics**

### **Description**

**c_funloc**(3) determines the C address of the argument.

### **Options**

- **x**
  : Interoperable function or pointer to such function.

### **Result**

The return value is of type c_funptr and contains the C address of the
argument.

### **Examples**

Sample program:

```fortran
! program demo_c_funloc and module
module x
use iso_c_binding
implicit none
contains
subroutine sub(a) bind(c)
real(c_float) :: a
   a = sqrt(a)+5.0
end subroutine sub
end module x
!
program demo_c_funloc
use iso_c_binding
use x
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
     import :: c_funptr
     type(c_funptr), intent(in) :: p
   end subroutine
end interface
   call my_routine(c_funloc(sub))
!
end program demo_c_funloc
```

### **Standard**

Fortran 2003

### **See Also**

[**c_associated**(3)](#c_associated),
[**c_loc**(3)](#c_loc),
[**c_f_pointer**(3)](#c_f_pointer),

[**c_f_procpointer**(3)](#c_f_procpointer),
**iso_c_binding**(3)

 _fortran-lang intrinsic descriptions_

### **Name**

**char**(3) - \[CHARACTER\] Character conversion function

### **Synopsis**
```fortran
    result = char(i [,kind])
```
```fortran
     elemental integer function char(i,kind)

      integer(kind=KIND),intent(in) :: c
      integer(kind=**),intent(in),optional :: kind
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

### **Description**

**char**(3) returns the character represented by the integer **i**.

### **Options**

- **i**
  : The type shall be _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _character_

### **Examples**

Sample program:

```fortran
program demo_char
implicit none
integer :: i = 74
character(1) :: c
    c = char(i)
    print *, i, c ! returns 'J'
end program demo_char
```

Results:

```text
             74 J
```

### **Note**

See [**ichar**(3)](#char) for a discussion of converting between numerical
values and formatted string representations.

### **Standard**

FORTRAN 77

### **See Also**

[**achar**(3)](#achar),
[**iachar**(3)](#iachar),
[**ichar**(3)](#ichar)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl), [**adjustr**(3)](#adjustr), [**index**(3)](#index),
  [**scan**(3)](#scan), [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions_

## c_loc

### **Name**

**c_loc**(3) - \[ISO_C_BINDING\] Obtain the C address of an object

### **Synopsis**
```fortran
    result = c_loc(x)
```
```fortran
```
### **Characteristics**

### **Description**

  **c_loc**(3) determines the C address of the argument.

### **Options**

- **x**
  : Shall have either the _pointer_ or _target_ attribute. It shall not be a
  coindexed object. It shall either be a variable with interoperable
  type and kind type parameters, or be a scalar, nonpolymorphic
  variable with no length type parameters.

### **Result**

The return value is of type c_ptr and contains the C address of the
argument.

### **Examples**

Sample program:

```fortran
   subroutine association_test(a,b)
   use iso_c_binding, only: c_associated, c_loc, c_ptr
   implicit none
   real, pointer :: a
   type(c_ptr) :: b
     if(c_associated(b, c_loc(a))) &
        stop 'b and a do not point to same target'
   end subroutine association_test
```

### **Standard**

Fortran 2003

### **See Also**

[**c_associated**(3)](#c_associated),
[**c_funloc**(3)](#c_funloc),
[**c_f_pointer**(3)](#c_f_pointer),

[**c_f_procpointer**(3)](#c_f_procpointer),
**iso_c_binding**(3)

 _fortran-lang intrinsic descriptions_

## cmplx

### **Name**

**cmplx**(3) - \[TYPE:NUMERIC\] Convert values to a complex type

### **Synopsis**
```fortran
    result = cmplx(x [,y] [,kind])
```
```fortran
     elemental complex(kind=KIND) function cmplx( x, y, kind )

      TYPE(kind=**),intent(in) :: x
      TYPE(kind=**),intent(in),optional :: y
      integer,intent(in),optional :: KIND
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- The type of **x** **TYPE** may be _integer_, _real_, or _complex_.
- **y** is allowed only if **x** is not _complex_. The **TYPE** for
  **y** may be _integer_ or _real_.
- **kind** is a constant _integer_ initialization expression indicating the kind
  parameter of the result.

The type of the arguments does not affect the kind of the result except
for a _complex_ **x** value.

- if **kind** is not present and **x** is _complex_ the result is of the kind
  of **x**.

- if **kind** is not present and **x** is not _complex_ the result if of default
  _complex_ kind.

### **Description**

The **cmplx**(3) function converts numeric values to a _complex_ value.

Even though constants can be used to define a complex variable using syntax like
```fortran
      z = (1.23456789, 9.87654321)
```
this will not work for variables. So you cannot enter
```fortran
      z = (a, b)  ! NO ! (unless a and b are constants, not variables)
```
so to construct a _complex_ value using non-complex values you must use
the **cmplx**(3) function:
```fortran
      z = cmplx(a, b)
```
or assign values separately to the imaginary and real components using
the **%IM** and **%RE** designators:
```fortran
      z%re = a
      z%im = b
```
If **x** is complex **y** is not allowed and **cmplx** essentially
returns the input value except for an optional change of kind, which can be
useful when passing a value to a procedure that requires the arguments
to have a different kind (and does not return an altered value):
```fortran
      call something(cmplx(z,kind=real64))
```
would pass a copy of a value with kind=real64 even if z had a different kind

but otherwise is equivalent to a simple assign. So if z1 and z2 were _complex_:
```fortran
      z2 = z1        ! equivalent statements
      z2 = cmplx(z1)
```
If **x** is not _complex_ **x** is only used to define the real component
of the result but **y** is still optional -- the imaginary part of the
result will just be assigned a value of zero.

If **y** is present it is converted to the imaginary component.

#### **cmplx(3) and double precision**

Primarily in order to maintain upward compatibility you need to be careful
when working with complex values of higher precision that the default.

It was necessary for Fortran to continue to specify that **cmplx**(3)
always return a result of the default kind if the **kind** option
is absent, since that is the behavior mandated by FORTRAN 77.

It might have been preferable to use the highest precision of the
arguments for determining the return kind, but that is not the case. So
with arguments with greater precision than default values you are
required to use the **kind** argument or the greater precision values
will be reduced to default precision.

This means **cmplx(d1,d2)**, where **d1** and **d2** are
_doubleprecision_, is treated as:
```fortran
      cmplx(sngl(d1), sngl(d2))
```
which looses precision.

So Fortran 90 extends the **cmplx**(3) intrinsic by adding an extra
argument used to specify the desired kind of the complex result.

```fortran
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
     ! wrong ways to specify constant values
      ! note this was stored with default real precision !
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)

      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note output components are just real
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      !
      ! YES
      !
      ! kind= makes it work
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
      print *, 'YES, Z8=',z8,real(z8),aimag(z8)
```
A more recent alternative to using **cmplx**(3) is "F2018 component
syntax" where real and imaginary parts of a complex entity can be
accessed independently:

```fortran
value%RE     ! %RE specifies the real part
or
value%IM     ! %IM specifies the imaginary part

````
Where the designator value is of course of complex type.

The type of a complex-part-designator is _real_, and its kind and shape
are those of the designator. That is, you retain the precision of the
complex value by default, unlike with **cmplx**.

The following are examples of complex part designators:

```fortran
       impedance%re           !-- Same value as _real_(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of x to zero
       x(1:2)%re=[10,20]      !-- even if x is an array
````

#### NOTE for I/O
  Note that if format statements are specified a complex value is
  treated as two real values.

  For list-directed I/O (ie. using an asterisk for a format) and NAMELIST
  output the values are expected to be delimited by "(" and ")" and of
  the form "(real_part,imaginary_part)". For NAMELIST input parenthesized
  values or lists of multiple _real_ values are acceptable.

### **Options**

- **x**
  : The value assigned to the _real_ component of the result when **x** is
  not complex.

  If **x** is complex, the result is the same as if the real part of the
  input was passed as **x** and the imaginary part as **y**.
```fortran
     result = CMPLX (REAL (X), AIMAG (X), KIND).
```
   That is, a complex **x** value is copied to the result value with a
   possible change of kind.

- **y**
  : **y* is only allowed if **x** is not _complex_. Its value
  is assigned to the imaginary component of the result and defaults
  to a value of zero if absent.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of _complex_ type, with magnitudes determined by the
values **x** and **y**.

The common case when **x** is not complex is that the real
component of the result is assigned the value of **x** and the imaginary
part is zero or the value of **y** if **y** is present.

When **x** is complex **y** is not allowed and the result is the same
value as **x** with a possible change of kind. That is, the real part
is **real(x, kind)** and the imaginary part is **real(y, kind)**.

### **Examples**

Sample program:

```fortran
program demo_aimag
implicit none
integer,parameter :: dp=kind(0.0d0)
real(kind=dp)    :: precise, z8
complex          :: z4, three(3)
   precise=1.2345678901234567d0

  ! basic
   z4 = cmplx(-3)
   print *, 'Z4=',z4
   z4 = cmplx(1.23456789, 1.23456789)
   print *, 'Z4=',z4
   ! with a format treat a complex as two real values
   print '(g0,1x,g0,1x,g0)','Z4=',z4

  ! working with higher precision values
   ! using kind=dp makes it keep DOUBLEPRECISION precision
   ! otherwise the result would be of default kind
   z8 = cmplx(precise, -precise )
   print *, 'lost precision Z8=',z8
   z8 = cmplx(precise, -precise ,kind=dp)
   print *, 'kept precision Z8=',z8

  ! assignment of constant values does not require cmplx(3)00
   ! The following is intuitive and works without calling cmplx(3)
   ! but does not work for variables just constants
   z8 = (1.1111111111111111d0, 2.2222222222222222d0 )
   print *, 'Z8 defined with constants=',z8

  ! elemental
   three=cmplx([10,20,30],-1)
   print *, 'three=',three

  ! descriptors are an alternative
   three(1:2)%re=[100,200]
   print *, 'three=',three

end program demo_aimag
```
Results:
```text
    Z4= (-3.000000,0.0000000E+00)
    Z4= (1.234568,1.234568)
   Z4= 1.234568 1.234568
    lost precision Z8=   1.23456788063049
    kept precision Z8=   1.23456789012346
    Z8 defined with constants=   1.11111111111111
    three= (10.00000,-1.000000) (20.00000,-1.000000) (30.00000,-1.000000)
    three= (100.0000,-1.000000) (200.0000,-1.000000) (30.00000,-1.000000)
```
### **Standard**

FORTRAN 77, KIND added in Fortran 90.

### **See Also**

- [**aimag**(3)](#aimag) - Imaginary part of complex number
- [**conjg**(3)](#conjg) - Complex conjugate function
- [**real**(3)](#real) - Convert to real type

Fortran has strong support for _complex_ values, including many intrinsics
that take or produce _complex_ values in addition to algebraic and
logical expressions:

[**abs**(3)](#abs),
[**acosh**(3)](#acosh),
[**acos**(3)](#acos),
[**asinh**(3)](#asinh),
[**asin**(3)](#asin),
[**atan2**(3)](#atan2),
[**atanh**(3)](#atanh),
[**atan**(3)](#atan),
[**cosh**(3)](#cosh),
[**cos**(3)](#cos),
[**co_sum**(3)](#co_sum),
[**dble**(3)](#dble),
[**dot_product**(3)](#dot_product),
[**exp**(3)](#exp),
[**int**(3)](#int),
[**is_contiguous**(3)](#is_contiguous),
[**kind**(3)](#kind),
[**log**(3)](#log),
[**matmul**(3)](#matmul),
[**precision**(3)](#precision),
[**product**(3)](#product),
[**range**(3)](#range),
[**rank**(3)](#rank),
[**sinh**(3)](#sinh),
[**sin**(3)](#sin),
[**sqrt**(3)](#sqrt),
[**storage_size**(3)](#storage_size),
[**sum**(3)](#sum),
[**tanh**(3)](#tanh),
[**tan**(3)](#tan),
[**unpack**(3)](#unpack),

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## co_broadcast

### **Name**

**co_broadcast**(3) - \[COLLECTIVE\] Copy a value to all images the current set of images

### **Synopsis**
```fortran
    call co_broadcast(a, source_image [,stat] [,errmsg] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_broadcast**(3) copies the value of argument **a** on the image with image
index source_image to all images in the current team. **a** becomes defined
as if by intrinsic assignment. If the execution was successful and **stat**
is present, it is assigned the value zero. If the execution failed, **stat**
gets assigned a nonzero value and, if present, **errmsg** gets assigned a
value describing the occurred error.

### **Options**

- **a**
  : **intent(inout)** argument; shall have the same dynamic type and
  type parameters on all images of the current team. If it is an
  array, it shall have the same shape on all images.

- **source_image**
  : a scalar integer expression. It shall have the same the same value
  on all images and refer to an image of the current team.

- **stat**
  : (optional) a scalar integer variable

- **errmsg**
  : (optional) a scalar character variable

### **Examples**

Sample program:

```fortran
program demo_co_broadcast
implicit none
integer :: val(3)
   if (this_image() == 1) then
      val = [1, 5, 3]
   endif
   call co_broadcast (val, source_image=1)
   print *, this_image(), ":", val
end program demo_co_broadcast
```
### **Standard**

Fortran xx

### **See Also**

[**co_max**(3)](#co_max),
[**co_min**(3)](#co_min),
[**co_sum**(3)](#co_sum),
[**co_reduce**(3)](#co_reduce)

 _fortran-lang intrinsic descriptions_

## co_lbound

### **Name**

**co_lbound**(3) - \[COLLECTIVE\] Lower codimension bounds of an array

### **Synopsis**
```fortran
     result = co_lbound( coarray [,dim] [,kind] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_lbound**(3) returns the lower bounds of a coarray, or a single
lower cobound along the **dim** codimension.

### **Options**

- **array**
  : Shall be an coarray, of any type.

- **dim**
  : (Optional) Shall be a scalar _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is absent, the
result is an array of the lower cobounds of **coarray**. If **dim** is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

### **Standard**

Fortran 2008

### **See Also**

[**co_ubound**(3)](#co_ubound),
[**lbound**(3)](#lbound)

 _fortran-lang intrinsic descriptions_

## co_max

### **Name**

**co_max**(3) - \[COLLECTIVE\] Maximal value on the current set of images

### **Synopsis**
```fortran
     call co_max(a, result_image [,stat] [,errmsg] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_max**(3) determines element-wise the maximal value of **a** on all
images of the current team. If result_image is present, the maximum values
are returned in **a** on the specified image only and the value of **a**
on the other images become undefined. If result_image is not present,
the value is returned on all images. If the execution was successful
and **stat** is present, it is assigned the value zero. If the execution
failed, **stat** gets assigned a nonzero value and, if present, **errmsg**
gets assigned a value describing the occurred error.

### **Options**

- **a**
  : shall be an integer, real or character variable, which has the same
  type and type parameters on all images of the team.

- **result_image**
  : (optional) a scalar integer expression; if present, it shall have
  the same the same value on all images and refer to an image of the
  current team.

- **stat**
  : (optional) a scalar integer variable

- **errmsg**
  : (optional) a scalar character variable

### **Examples**

Sample program:

```fortran
program demo_co_max
implicit none
integer :: val
   val = this_image()
   call co_max(val, result_image=1)
   if (this_image() == 1) then
     write(*,*) "Maximal value", val  ! prints num_images()
   endif
end program demo_co_max
```

Results:

```text
    Maximal value           2
```

### **Standard**

TS 18508

### **See Also**

[**co_min**(3)](#co_min),
[**co_sum**(3)](#co_sum),
[**co_reduce**(3)](#co_reduce),
[**co_broadcast**(3)](#co_broadcast)

 _fortran-lang intrinsic descriptions_

## co_min

### **Name**

**co_min**(3) - \[COLLECTIVE\] Minimal value on the current set of images

### **Synopsis**
```fortran
     call co_min(a, result_image [,stat] [,errmsg] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_min**(3) determines element-wise the minimal value of **a** on all
images of the current team. If result_image is present, the minimal values
are returned in **a** on the specified image only and the value of **a**
on the other images become undefined. If result_image is not present,
the value is returned on all images. If the execution was successful
and **stat** is present, it is assigned the value zero. If the execution
failed, **stat** gets assigned a nonzero value and, if present, **errmsg**
gets assigned a value describing the occurred error.

### **Options**

- **a**
  : shall be an integer, real or character variable, which has the same
  type and type parameters on all images of the team.

- **result_image**
  : (optional) a scalar integer expression; if present, it shall have
  the same the same value on all images and refer to an image of the
  current team.

- **stat**
  : (optional) a scalar integer variable

- **errmsg**
  : (optional) a scalar character variable

### **Examples**

Sample program:

```fortran
program demo_co_min
implicit none
integer :: val
   val = this_image()
   call co_min(val, result_image=1)
   if (this_image() == 1) then
     write(*,*) "Minimal value", val  ! prints 1
   endif
end program demo_co_min
```

### **Standard**

TS 18508

### **See Also**

[**co_max**(3)](#co_max),
[**co_sum**(3)](#co_sum),
[**co_reduce**(3)](#co_reduce),
[**co_broadcast**(3)](#co_broadcast)

 _fortran-lang intrinsic descriptions_

## command_argument_count

### **Name**

**command_argument_count**(3) - \[SYSTEM:COMMAND LINE\] Get number of command line arguments

### **Synopsis**
```fortran
    result = command_argument_count()
```
```fortran
     integer function command_argument_count()
```
### **Characteristics**

### **Description**

**command_argument_count**(3) returns the number of arguments passed
on the command line when the containing program was invoked.

### **Options**

None

### **Result**

- **count**
  : The return value is of type default _integer_. It is the number of
  arguments passed on the command line when the program was invoked.

### **Examples**

Sample program:

```fortran
program demo_command_argument_count
implicit none
integer :: count
   count = command_argument_count()
   print *, count
end program demo_command_argument_count
```

Sample output:

```bash
   # the command verb does not count
   ./test_command_argument_count
       0
   # quoted strings may count as one argument
   ./test_command_argument_count count arguments
       2
   ./test_command_argument_count 'count arguments'
       1
```

### **Standard**

Fortran 2003

### **See Also**

[**get_command**(3)](#get_command),
[**get_command_argument**(3)](#get_command_argument)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## compiler_options

### **Name**

**compiler_options**(3) - \[COMPILER INQUIRY\] Options passed to the compiler

### **Synopsis**
```fortran
    result = compiler_options()
```
```fortran
     character(len=:) function compiler_options()
```
### **Characteristics**

### **Description**

**compiler_options**(3) returns a string with the options used for compiling.

### **Options**

None.

### **Result**

The return value is a default-kind string with system-dependent length.
It contains the compiler flags used to compile the file, which called
the compiler_options intrinsic.

### **Examples**

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
   print '(4a)', &
      'This file was compiled by ', &
      compiler_version(),           &
      ' using the options ',        &
      compiler_options()
end program demo_compiler_version
```

Results:

```
   This file was compiled by GCC version 5.4.0 using the options
   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall
   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan
   -fno-range-check -frecord-marker=4
   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
```

### **Standard**

Fortran 2008

### **See Also**

[**compiler_version**(3)](#compiler_version),
**iso_fortran_env**(7)

 _fortran-lang intrinsic descriptions_

## compiler_version

### **Name**

**compiler_version**(3) - \[COMPILER INQUIRY\] Compiler version string

### **Synopsis**
```fortran
    result = compiler_version()
```
```fortran
     character(len=:) function compiler_version()
```
### **Characteristics**

### **Description**

**compiler_version**(3) returns a string containing the name and
version of the compiler.

### **Options**

None.

### **Result**

The return value is a default-kind string with system-dependent length.
It contains the name of the compiler and its version number.

### **Examples**

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
   print '(4a)', &
      'This file was compiled by ', &
      compiler_version(),           &
      ' using the options ',        &
      compiler_options()
end program demo_compiler_version
```

Results:

```
   This file was compiled by GCC version 5.4.0 using the options
   -I /usr/include/w32api -I /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
   -mtune=generic -march=x86-64 -g -Wunused -Wuninitialized -Wall
   -std=f2008 -fbounds-check -fbacktrace -finit-real=nan
   -fno-range-check -frecord-marker=4
   -J /home/urbanjs/V600/lib/CYGWIN64_GFORTRAN
```

### **Standard**

Fortran 2008

### **See Also**

[**compiler_options**(3)](#compiler_options),
**iso_fortran_env**(7)

 _fortran-lang intrinsic descriptions_

## conjg

### **Name**

**conjg**(3) - \[NUMERIC\] Complex conjugate of a complex value

### **Synopsis**
```fortran
    result = conjg(z)
```
```fortran
     elemental complex(kind=KIND) function conjg(z)

      complex(kind=KIND),intent(in) :: z
```
### **Characteristics**

- **KIND** may be any supported value for the _complex_ input value.

- The returned value with be the same _complex_ type as the input.

### **Description**

**conjg**(3) returns the complex conjugate of the _complex_ value **z**.

In mathematics, the complex conjugate of a complex number is a value
whose real and imaginary part are equal parts are equal in magnitude to
each other but the **y** value has opposite sign.

That is, If **z** is the _complex_ value **(x, y)** then the result is
**(x, -y)**.

For matrices of complex numbers, **conjg(array)** represents the
element-by-element conjugation of **array**; not the conjugate transpose
of the **array** .

### **Options**

- **z**
  : The _complex_ value to create the conjugate of.

### **Result**

Returns a complex value equal to the input value except the sign of
the imaginary component is the opposite of the input value.

### **Examples**

Sample program:

```fortran
program demo_conjg
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
complex :: z = (2.0, 3.0)
complex(kind=real64) :: dz = (   &
   &  1.2345678901234567_real64, -1.2345678901234567_real64)
complex :: arr(3,3)
integer :: i
   ! basics
    ! notice the sine of the imaginary component changes
    print *, z, conjg(z)

    ! any complex kind is supported. z is of default kind but
    ! dz is kind=real64.
    print *, dz
    dz = conjg(dz)
    print *, dz
    print *

    ! the function is elemental so it can take arrays
    arr(1,:)=[(-1.0, 2.0),( 3.0, 4.0),( 5.0,-6.0)]
    arr(2,:)=[( 7.0,-8.0),( 8.0, 9.0),( 9.0, 9.0)]
    arr(3,:)=[( 1.0, 9.0),( 2.0, 0.0),(-3.0,-7.0)]

    write(*,*)'original'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)
    arr = conjg(arr)
    write(*,*)'conjugate'
    write(*,'(3("(",g8.2,",",g8.2,")",1x))')(arr(i,:),i=1,3)

end program demo_conjg
```
Results:
```fortran
 (2.000000,3.000000) (2.000000,-3.000000)

 (1.23456789012346,-1.23456789012346)
 (1.23456789012346,1.23456789012346)

 original
(-1.0    , 2.0    ) ( 3.0    , 4.0    ) ( 5.0    ,-6.0    )
( 7.0    ,-8.0    ) ( 8.0    , 9.0    ) ( 9.0    , 9.0    )
( 1.0    , 9.0    ) ( 2.0    , 0.0    ) (-3.0    ,-7.0    )

 conjugate
(-1.0    ,-2.0    ) ( 3.0    ,-4.0    ) ( 5.0    , 6.0    )
( 7.0    , 8.0    ) ( 8.0    ,-9.0    ) ( 9.0    ,-9.0    )
( 1.0    ,-9.0    ) ( 2.0    , 0.0    ) (-3.0    , 7.0    )
```
### **Standard**

FORTRAN 77

### **See Also**

- [**aimag**(3)](#aimag) - Imaginary part of complex number
- [**cmplx**(3)](#cmplx) - Complex conversion function
- [**real**(3)](#real) - Convert to real type

Fortran has strong support for _complex_ values, including many intrinsics
that take or produce _complex_ values in addition to algebraic and
logical expressions:

[**abs**(3)](#abs),
[**acosh**(3)](#acosh),
[**acos**(3)](#acos),
[**asinh**(3)](#asinh),
[**asin**(3)](#asin),
[**atan2**(3)](#atan2),
[**atanh**(3)](#atanh),
[**atan**(3)](#atan),
[**cosh**(3)](#cosh),
[**cos**(3)](#cos),
[**co_sum**(3)](#co_sum),
[**dble**(3)](#dble),
[**dot_product**(3)](#dot_product),
[**exp**(3)](#exp),
[**int**(3)](#int),
[**is_contiguous**(3)](#is_contiguous),
[**kind**(3)](#kind),
[**log**(3)](#log),
[**matmul**(3)](#matmul),
[**precision**(3)](#precision),
[**product**(3)](#product),
[**range**(3)](#range),
[**rank**(3)](#rank),
[**sinh**(3)](#sinh),
[**sin**(3)](#sin),
[**sqrt**(3)](#sqrt),
[**storage_size**(3)](#storage_size),
[**sum**(3)](#sum),
[**tanh**(3)](#tanh),
[**tan**(3)](#tan),
[**unpack**(3)](#unpack),

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## co_reduce

### **Name**

**co_reduce**(3) - \[COLLECTIVE\] Reduction of values on the current set of images

### **Synopsis**
```fortran
    call co_reduce(a, operation, result_image [,stat] [,errmsg] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_reduce**(3) determines element-wise the reduction of the value of **a** on
all images of the current team. The pure function passed as **operation** is
used to pairwise reduce the values of **a** by passing either the value of **a**
of different images or the result values of such a reduction as
argument. If **a** is an array, the reduction is done element wise. If
result_image is present, the result values are returned in **a** on the
specified image only and the value of **a** on the other images become
undefined. If result_image is not present, the value is returned on all
images. If the execution was successful and **stat** is present, it is
assigned the value zero. If the execution failed, **stat** gets assigned a
nonzero value and, if present, **errmsg** gets assigned a value describing
the occurred error.

### **Options**

- **a**
  : is an **intent(inout)** argument and shall be nonpolymorphic. If it
  is allocatable, it shall be allocated; if it is a pointer, it shall
  be associated. **a** shall have the same type and type parameters on all
  images of the team; if it is an array, it shall have the same shape
  on all images.

- **operation**
  : pure function with two scalar nonallocatable arguments, which shall
  be nonpolymorphic and have the same type and type parameters as **a**.
  The function shall return a nonallocatable scalar of the same type
  and type parameters as **a**. The function shall be the same on all
  images and with regards to the arguments mathematically commutative
  and associative. Note that OPERATION may not be an elemental unless
  it is an intrinsic function.

- **result_image**

  : (optional) a scalar integer expression; if present, it shall
  have the same the same value on all images and refer to an image
  of the current team.

- **stat**
  : (optional) a scalar integer variable

- **errmsg**
  : (optional) a scalar character variable

### **Examples**

Sample program:

```fortran
program demo_co_reduce
implicit none
integer :: val

   val = this_image()
   call co_reduce(val, myprod, 1)
   if (this_image() == 1) then
      write(*,*) "Product value", val  ! prints num_images() factorial
   endif

contains

pure function myprod(a, b)
   integer, value :: a, b
   integer :: myprod
   myprod = a * b
end function myprod

end program demo_co_reduce
```

### **Note**

While the rules permit in principle an intrinsic function, none of the
intrinsics in the standard fulfill the criteria of having a specific
function, which takes two arguments of the same type and returning that
type as a result.

### **Standard**

TS 18508

### **See Also**

[**co_min**(3)](#co_min),
[**co_max**(3)](#co_max),
[**co_sum**(3)](#co_sum),
[**co_broadcast**(3)](#co_broadcast)

 _fortran-lang intrinsic descriptions_

## cosh

### **Name**

**cosh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function

### **Synopsis**
```fortran
    result = cosh(x)
```
```fortran
     elemental TYPE(kind=KIND) function cosh(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_ of any kind.
 -  The returned value will be of the same type and kind as the argument.

### **Description**

**cosh**(3) computes the hyperbolic cosine of **x**.

### **Options**

- **x**
  : the value to compute the hyerbolic cosine of

### **Result**

  If **x** is _complex_, the imaginary part of the result is in radians.

  If **x** is _real_, the return value has a lower bound of one,
  **cosh(x) \>= 1**.

### **Examples**

Sample program:
```fortran
program demo_cosh
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = cosh(x)
end program demo_cosh
```
### **Standard**

FORTRAN 77 , for a complex argument - Fortran 2008

### **See Also**

Inverse function: [**acosh**(3)](#acosh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_

## cos

### **Name**

**cos**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function

### **Synopsis**
```fortran
    result = cos(x)
```
```fortran
     elemental TYPE(kind=KIND) function cos(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - where **TYPE** may be _real_ or _complex_ of any kind.
 - the returned value will be of the same type and kind as the argument.

### **Description**

  **cos**(3) computes the cosine of an angle **x** given the size of
  the angle in radians.

  The cosine of a _real_ value is the ratio of the adjacent side to the
  hypotenuse of a right-angled triangle.

### **Options**

- **x**
  : The angle in radians to compute the cosine of.

### **Result**

  The return value is the tangent of **x**.

  If **x** is of the type _real_, the return value lies in
  the range **-1 \<= cos(x) \<= 1** .

### **Examples**

Sample program:
```fortran
program demo_cos
implicit none
doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
end program demo_cos
```
Results:
```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```
### **Standard**

FORTRAN 77

### **See Also**

[**acos**(3)](#acos),
[**sin**(3)](#sin),
[**tan**(3)](#tan)

### **Resources**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

 _fortran-lang intrinsic descriptions_

## co_sum

### **Name**

**co_sum**(3) - \[COLLECTIVE\] Sum of values on the current set of images

### **Synopsis**
```fortran
    call co_sum(a, result_image [,stat] [,errmsg] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_sum**(3) sums up the values of each element of **a** on all images
of the current team.

If result_image is present, the summed-up values are returned in **a**
on the specified image only and the value of **a** on the other images
become undefined.

If result_image is not present, the value is returned on all images. If
the execution was successful and **stat** is present, it is assigned the
value zero. If the execution failed, **stat** gets assigned a nonzero
value and, if present, **errmsg** gets assigned a value describing the
occurred error.

### **Options**

- **a**
  : shall be an integer, real or complex variable, which has the same
  type and type parameters on all images of the team.

- **result_image**
  : (optional) a scalar integer expression; if present, it shall have
  the same the same value on all images and refer to an image of the
  current team.

- **stat**
  : (optional) a scalar integer variable

- **errmsg**
  : (optional) a scalar character variable

### **Examples**

Sample program:

```fortran
program demo_co_sum
implicit none
integer :: val
   val = this_image()
   call co_sum(val, result_image=1)
   if (this_image() == 1) then
      ! prints (n**2 + n)/2, with n = num_images()
      write(*,*) "The sum is ", val
   endif
end program demo_co_sum
```

Results:

```text
    The sum is            1
```

### **Standard**

TS 18508

### **See Also**

[**co_max**(3)](#co_max),
[**co_min**(3)](#co_min),
[**co_reduce**(3)](#co_reduce),
[**co_broadcast**(3)](#co_broadcast)

 _fortran-lang intrinsic descriptions_

## co_ubound

### **Name**

**co_ubound**(3) - \[COLLECTIVE\] Upper codimension bounds of an array

### **Synopsis**
```fortran
    result = co_ubound(coarray [,dim] [,kind] )
```
```fortran
```
### **Characteristics**

### **Description**

**co_ubound**(3) returns the upper cobounds of a coarray, or a single
upper cobound along the **dim** codimension.

### **Options**

- **array**
  : Shall be an coarray, of any type.

- **dim**
  : (Optional) Shall be a scalar _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is absent, the
result is an array of the lower cobounds of **coarray**. If **dim** is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

### **Standard**

Fortran 2008

### **See Also**

[**co_lbound**(3)](#co_lbound),
[**lbound**(3)](#lbound),
[**ubound**(3)](#ubound)

 _fortran-lang intrinsic descriptions_

## count

### **Name**

**count**(3) - \[ARRAY REDUCTION\] Count true values in an array

### **Synopsis**
```fortran
    result = count(mask [,dim] [,kind] )
```
```fortran
     integer(kind=KIND) function count(mask, dim, kind )

      logical(kind=**),intent(in) :: mask(..)
      integer(kind=**),intent(in),optional :: dim
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

  - **mask** is an array of any shape.

  - If **dim** is present, the result is an array with the specified rank
    removed.

  - The return value is of default _integer_ type unless **kind** is specified
    to declare the kind of the result.

### **Description**

  **count**(3) counts the number of _.true._ elements in a logical
  **mask**, or, if the **dim** argument is supplied, counts the number
  of elements along each row of the array in the **dim** direction. If
  the array has zero size or all of the elements of **mask** are false,
  then the result is **0**.

### **Options**

- **mask**
  : an array to count the number of _.true._ values in

- **dim**
  : specifies to remove this dimension from the result and produce an
    array of counts of _.true._ values along the removed dimension.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is the number of _.true_. values in **mask** if **dim**
is not present.

If **dim** is present, the result is an array with a rank one less
than the rank of the input array **mask**, and a size corresponding
to the shape of **array** with the **dim** dimension removed, with the
remaining elements containing the number of _.true._ elements along the
removed dimension.

### **Examples**

Sample program:

```fortran
program demo_count
implicit none
character(len=*),parameter :: ints='(*(i2,1x))'
! two arrays and a mask all with the same shape
integer, dimension(2,3) :: a, b
logical, dimension(2,3) :: mymask
integer :: i
integer :: c(2,3,4)
   a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
   b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
   c = reshape( [( i,i=1,24)], [ 2, 3 ,4])
  ! show numeric arrays we will compare
   print '(3i3)', a(1,:)
   print '(3i3)', a(2,:)
   print *
   print '(3i3)', b(1,:)
   print '(3i3)', b(2,:)

  ! basic calls
   print *, 'count a few basic things ...'
   print *, 'count a>b',count(a>b)
   print *, 'count b<a',count(a<b)
   print *, 'count b==a',count(a==b)
   print *, 'check sum = ',count(a>b) + &
                         & count(a<b) + &
                         & count(a==b).eq.size(a)

   ! The common usage is just getting a count, but if you want
   ! to specify the DIM argument and get back reduced arrays
   ! of counts this is easier to visualize if we look at a mask
   ! make a mask identifying unequal elements
   mymask = a.ne.b
   print *, 'show mask for a.ne.b'
   print '(3l3)', mymask(1,:)
   print '(3l3)', mymask(2,:)
   ! count total and along rows and columns

   print '(a)', 'number of elements not equal'
   print '(a)', '(ie. total true elements in the mask)'
   print '(3i3)', count(mymask)

   print '(a)', 'count of elements not equal in each column'
   print '(a)', '(ie. total true elements in each column)'
   print '(3i3)', count(mymask, dim=1)

   print '(a)', 'count of elements not equal in each row'
   print '(a)', '(ie. total true elements in each row)'
   print '(3i3)', count(mymask, dim=2)

   ! working with rank=3 ...
   print *, 'lets try this with c(2,3,4)'
   print *,'  taking the result of the modulo   '
   print *,'   z=1      z=2      z=3      z=4   '
   print *,'  1 3 0 || 2 4 1 || 3 0 2 || 4 1 3 |'
   print *,'  2 4 1 || 3 0 2 || 4 1 3 || 0 2 4 |'
   print *,'                                    '
   print *,'  would result in the mask ..       '
   print *,'  F F T || F F F || F T F || F F F |'
   print *,'  F F F || F T F || F F F || T F F |'
   print *,'                                    '
   print *,' the total number of .true.values is'
   print ints, count(modulo(c,5).eq.0)
   call printi('counting up along a row and removing rows',&
   count(modulo(c,5).eq.0,dim=1))
   call printi('counting up along a column and removing columns',&
   count(modulo(c,5).eq.0,dim=2))
   call printi('counting up along a depth and removing depths',&
   count(modulo(c,5).eq.0,dim=3))

contains

   ! CONVENIENCE ROUTINE FOR PRINTING SMALL INTEGER MATRICES
   subroutine printi(title,arr)
   implicit none

   !@(#) print small 2d integer arrays in row-column format

   character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
   character(len=*),intent(in)  :: title
   integer,intent(in)           :: arr(:,:)
   integer                      :: i
   character(len=:),allocatable :: biggest

      print all
      print all, trim(title),':(',shape(arr),')'  ! print title
      biggest='           '  ! make buffer to write integer into
      ! find how many characters to use for integers
      write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
      ! use this format to write a row
      biggest='(" > [",*(i'//trim(biggest)//':,","))'
      ! print one row of array at a time
      do i=1,size(arr,dim=1)
         write(*,fmt=biggest,advance='no')arr(i,:)
         write(*,'(" ]")')
      enddo

   end subroutine printi
end program demo_count
```
Results:
```text
     1  3  5
     2  4  6

     0  3  5
     7  4  8
    count a few basic things ...
    count a>b           1
    count b<a           2
    count b==a           3
    check sum =  T
    show mask for a.ne.b
     T  F  F
     T  F  T
   number of elements not equal
   (ie. total true elements in the mask)
     3
   count of elements not equal in each column
   (ie. total true elements in each column)
     2  0  1
   count of elements not equal in each row
   (ie. total true elements in each row)
     1  2
    lets try this with c(2,3,4)
      taking the result of the modulo
       z=1      z=2      z=3      z=4
      1 3 0 || 2 4 1 || 3 0 2 || 4 1 3 |
      2 4 1 || 3 0 2 || 4 1 3 || 0 2 4 |

      would result in the mask ..
      F F T || F F F || F T F || F F F |
      F F F || F T F || F F F || T F F |

     the total number of .true.values is
    4

   counting up along a row and removing rows :( 3 4 )
    > [ 0, 0, 0, 1 ]
    > [ 0, 1, 1, 0 ]
    > [ 1, 0, 0, 0 ]

   counting up along a column and removing columns :( 2 4 )
    > [ 1, 0, 1, 0 ]
    > [ 0, 1, 0, 1 ]

   counting up along a depth and removing depths :( 2 3 )
    > [ 0, 1, 1 ]
    > [ 1, 1, 0 ]
```
### **Standard**

Fortran 95 , with KIND argument - Fortran 2003

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## cpu_time

### **Name**

**cpu_time**(3) - \[SYSTEM:TIME\] Return CPU processor time in seconds

### **Synopsis**
```fortran
     call cpu_time(time)
```
```fortran
      subroutine cpu_time(time)

       real,intent(out) :: time
```
### **Characteristics**

  - **time** is of type _real_ and any kind, with **intent(out)**.

### **Description**

**cpu_time**(3) returns a _real_ value representing the elapsed CPU time
in seconds. This is useful for testing segments of code to determine
execution time.

The exact definition of time is left imprecise because of the variability
in what different processors are able to provide.

If no time source is available, TIME is set to a negative value.

Note that TIME may contain a system dependent, arbitrary offset and may
not start with 0.0. For **cpu_time**(3) the absolute value is meaningless.
Only differences between subsequent calls, as shown in the example below,
should be used.

A processor for which a single result is inadequate (for example, a
parallel processor) might choose to provide an additional version for
which **time** is an array.

### **Result**

- **time**
  : The type shall be _real_ with **intent(out)**. It is assigned a
  processor-dependent approximation to the processor time in seconds.
  If the processor cannot return a meaningful time, a
  processor-dependent negative value is returned.

  : The start time is left imprecise because the purpose is to time
  sections of code, as in the example. This might or might not
  include system overhead time.

### **Examples**

Sample program:

```fortran
program demo_cpu_time
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
real :: start, finish
real(kind=real64) :: startd, finishd
   !
   call cpu_time(start)
   call cpu_time(startd)
   ! put code to time here
   call cpu_time(finish)
   call cpu_time(finishd)
   !
  ! writes processor time taken by the piece of code.

  ! the accuracy of the clock and whether it includes system time
  ! as well as user time is processor dependent. Accuracy up to
  ! milliseconds is common but not guaranteed, and may be much
  ! higher or lower
   print '("Processor Time = ",f6.3," seconds.")',finish-start

   ! see your specific compiler documentation for how to measure
   ! parallel jobs and for the precision of the time returned
   print '("Processor Time = ",g0," seconds.")',finish-start
   print '("Processor Time = ",g0," seconds.")',finishd-startd
end program demo_cpu_time
```
Results:

  The precision of the result, some aspects of what is returned,
  and what if any options there are for parallel applications
  may very from system to system. See compiler-specific for details.
```text
   Processor Time =  0.000 seconds.
   Processor Time = .4000030E-05 seconds.
   Processor Time = .2000000000000265E-05 seconds.
```
### **Standard**

Fortran 95

### **See Also**

[**system_clock**(3)](#system_clock),
[**date_and_time**(3)](#date_and_time)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## cshift

### **Name**

**cshift**(3) - \[TRANSFORMATIONAL\] Circular shift elements of an array

### **Synopsis**
```fortran
    result = cshift(array, shift, dim)
```
```fortran
     type(TYPE, kind=KIND) function cshift(array, shift, dim )

      type(TYPE,kind=KIND),intent(in) :: array(..)
      integer(kind=IKIND),intent(in)  :: shift
      integer(kind=IKIND),intent(in)  :: dim
```
### **Characteristics**

  where **array** may be any type and rank (and the result will
  automatically be of the same type, kind and rank as **array**).

  The _kind_ of **shift** and **dim** may differ and be any supported
  value.

### **Description**

  **cshift**(3) performs a circular shift on elements
  of **array** along the dimension of **dim**. If **dim** is omitted it is
  taken to be **1**. **dim** is a scalar of type _integer_ in the range of
  **1 \<= dim \<= n**, where "n" is the rank of **array**.

  If the rank of
  **array** is one, then all elements of **array** are shifted by **shift**
  places. If rank is greater than one, then all complete rank one sections
  of **array** along the given dimension are shifted. Elements shifted
  out one end of each rank one section are shifted back in the other end.

### **Options**

- **array**
  : Shall be an array of any type.

- **shift**
  : The type shall be _integer_.

- **dim**
  : The type shall be _integer_.

### **Result**

Returns an array of same type and rank as the **array** argument.

### **Examples**

Sample program:

```fortran
program demo_cshift
implicit none
integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = cshift(a, SHIFT=[1, 2, -1], DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_cshift
```
Results:

```text
     1  4  7
     2  5  8
     3  6  9

     4  7  1
     8  2  5
     9  3  6
```
### **Standard**

Fortran 95

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## c_sizeof

### **Name**

**c_sizeof**(3) - \[ISO_C_BINDING\] Size in bytes of an expression

### **Synopsis**
```fortran
    result = c_sizeof(x)
```
```fortran
```
### **Characteristics**

### **Description**

**c_sizeof**(3) calculates the number of bytes of storage the
expression **x** occupies.

### **Options**

- **x**
  : The argument shall be an interoperable data entity.

### **Result**

The return value is of type integer and of the system-dependent kind
c*size_t (from the \_iso_c_binding* module). Its value is the
number of bytes occupied by the argument. If the argument has the
_pointer_ attribute, the number of bytes of the storage area pointed to is
returned. If the argument is of a derived type with _pointer_ or
_allocatable_ components, the return value does not account for the sizes
of the data pointed to by these components.

### **Examples**

Sample program:

```fortran
program demo_c_sizeof
use iso_c_binding
implicit none
real(c_float) :: r, s(5)
   print *, (c_sizeof(s)/c_sizeof(r) == 5)
end program demo_c_sizeof
```

Results:

```text
    T
```

The example will print _.true._ unless you are using a platform where
default _real_ variables are unusually padded.

### **Standard**

Fortran 2008

### **See Also**

[**storage_size**(3)](#storage_size)

 _fortran-lang intrinsic descriptions_

## date_and_time

### **Name**

**date_and_time**(3) - \[SYSTEM:TIME\] Gets current time

### **Synopsis**
```fortran
    call date_and_time( [date] [,time] [,zone] [,values] )
```
```fortran
     subroutine date_and_time(date, time, zone, values)

      character(len=8),intent(out),optional :: date
      character(len=10),intent(out),optional :: time
      character(len=5),intent(out),optional :: zone
      integer,intent(out),optional :: values(8)
```
### **Characteristics**

### **Description**

**date_and_time**(3) gets the corresponding
date and time information from the real-time system clock.

Unavailable time and date _character_ parameters return blanks.

### **Options**

- **date**
  : A character string of default kind of the form CCYYMMDD, of length 8 or larger.

- **time**
  : A character string of default kind of the form HHMMSS.SSS, of length 10 or larger.

- **zone**
  : A character string of default kind of the form (+-)HHMM, of length 5 or larger,
  representing the difference with respect to Coordinated Universal Time (UTC).

- **values**
  : An _integer_ array of eight elements that contains:

   - **values**(1) : The year
   - **values**(2) : The month
   - **values**(3) : The day of the month
   - **values**(4) : Time difference with UTC in minutes
   - **values**(5) : The hour of the day
   - **values**(6) : The minutes of the hour
   - **values**(7) : The seconds of the minute
   - **values**(8) : The milliseconds of the second

### **Examples**

Sample program:

```fortran
program demo_date_and_time
implicit none
character(len=8)     :: date
character(len=10)    :: time
character(len=5)     :: zone
integer,dimension(8) :: values

    call date_and_time(date,time,zone,values)

    ! using keyword arguments
    call date_and_time(DATE=date,TIME=time,ZONE=zone)
    print '(*(g0))','DATE="',date,'" TIME="',time,'" ZONE="',zone,'"'

    call date_and_time(VALUES=values)
    write(*,'(i5,a)') &
     & values(1),' - The year', &
     & values(2),' - The month', &
     & values(3),' - The day of the month', &
     & values(4),' - Time difference with UTC in minutes', &
     & values(5),' - The hour of the day', &
     & values(6),' - The minutes of the hour', &
     & values(7),' - The seconds of the minute', &
     & values(8),' - The milliseconds of the second'
end program demo_date_and_time
```

Results:

```
   DATE="20201222" TIME="165738.779" ZONE="-0500"
    2020 - The year
      12 - The month
      22 - The day of the month
    -300 - Time difference with UTC in minutes
      16 - The hour of the day
      57 - The minutes of the hour
      38 - The seconds of the minute
     779 - The milliseconds of the second
```

### **Standard**

Fortran 95

### **See Also**

[**cpu_time**(3)](#cpu_time),
[**system_clock**(3)](#system_clock)

### **Resources**

date and time conversion, formatting and computation

- [M_time](https://github.com/urbanjost/M_time)
- [datetime](https://github.com/wavebitscientific/datetime-fortran)
- [datetime-fortran](https://github.com/wavebitscientific/datetime-fortran)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## dble

### **Name**

**dble**(3) - \[TYPE:NUMERIC\] Double conversion function

### **Synopsis**
```fortran
    result = dble(a)
```
```fortran
     elemental doubleprecision function dble(a)

      doubleprecision :: dble
      TYPE(kind=KIND),intent(in) :: a
```
### **Characteristics**

where TYPE may be _integer_, _real_, or _complex_ and KIND any kind
supported by the TYPE.

### **Description**

**dble**(3) Converts **a** to double precision _real_ type.

### **Options**

- **a**
  : The type shall be _integer_, _real_, or _complex_.

### **Result**

The return value is of type _doubleprecision_. For _complex_ input,
the returned value has the magnitude and sign of the real component
of the input value.

### **Examples**

Sample program:

```fortran
program demo_dble
implicit none
real:: x = 2.18
integer :: i = 5
complex :: z = (2.3,1.14)
   print *, dble(x), dble(i), dble(z)
end program demo_dble
```

Results:

```text
  2.1800000667572021  5.0000000000000000   2.2999999523162842
```

### **Standard**

FORTRAN 77

### **See Also**

[**float**(3)](#float),
[**real**(3)](#real)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## digits

### **Name**

**digits**(3) - \[NUMERIC MODEL\] Significant digits function

### **Synopsis**
```fortran
    result = digits(x)
```
```fortran
     integer function digits(x)

      TYPE(kind=KIND),intent(in) :: x(..)
```
### **Characteristics**

where TYPE may be _integer_ or _real_ and KIND is any kind supported by
TYPE.

The return value is of type _integer_ of default kind.

### **Description**

**digits**(3) returns the number of significant digits of the internal
model representation of **x**. For example, on a system using a 32-bit
floating point representation, a default real number would likely return 24.

### **Options**

- **x**
  : The type may be a scalar or array of type _integer_ or _real_.

### **Result**

The return value is of type _integer_ of default kind.

### **Examples**

Sample program:

```fortran
program demo_digits
implicit none
integer :: i = 12345
real :: x = 3.143
doubleprecision :: y = 2.33d0
   print *,'default integer:', digits(i)
   print *,'default real:   ', digits(x)
   print *,'default doubleprecision:', digits(y)
end program demo_digits
```

Typical Results:

```
    default integer:                  31
    default real:                     24
    default doubleprecision:          53
```

### **Standard**

Fortran 95

### **See Also**

[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
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

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## dim

### **Name**

**dim**(3) - \[NUMERIC\] Positive difference

### **Synopsis**
```fortran
    result = dim(x, y)
```
```fortran
     elemental TYPE(kind=KIND) function dim(x, y)

      TYPE(kind=KIND),intent(in) :: x, y
```
### **Characteristics**

where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.

### **Description**

**dim**(3) returns the difference **x - y** if the result is positive;
otherwise it returns zero. It is equivalent to **max(0,x-y)** where the
arguments are all of the same type.

### **Options**

- **x**
  : The type shall be _integer_ or _real_

- **y**
  : The type shall be the same type and kind as **x**.

### **Result**

Returns the difference **xx -y** or zero, whichever is larger.
The return value is the same type and kind as the input arguments **x**
and **y**.

### **Examples**

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer           :: i
real(kind=real64) :: x

   ! basic usage
    i = dim(4, 15)
    x = dim(4.321_real64, 1.111_real64)
    print *, i
    print *, x

   ! elemental
    print *, dim([1,2,3],2)
    print *, dim([1,2,3],[3,2,1])
    print *, dim(-10,[0,-10,-20])

end program demo_dim
```
Results:
```text
              0
      3.21000000000000
              0           0           1
              0           0           2
              0           0          10
```
### **Standard**

FORTRAN 77

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## dot_product

### **Name**

**dot_product**(3) - \[TRANSFORMATIONAL\] Dot product function

### **Synopsis**
```fortran
    result = dot_product(vector_a, vector_b)
```
```fortran
     TYPE(kind=KIND) function dot_product(vector_a, vector_b)

      TYPE(kind=KIND),intent(in) :: vector_a(:)
      TYPE(kind=KIND),intent(in) :: vector_b(:)
```
### **Characteristics**

The two vectors may be either numeric or logical and must be arrays
of rank one and of equal size.

### **Description**

**dot_product**(3) computes the dot product
multiplication of two vectors **vector_a** and **vector_b**.

If the vectors are _integer_ or _real_, the result is
```fortran
     sum(vector_a*vector_b)
```
If the vectors are _complex_, the result is
```fortran
     sum(conjg(vector_a)*vector_b)**
```
If the vectors are _logical_, the result is
```fortran
     any(vector_a .and. vector_b)
```
### **Options**

- **vector_a**
  : The type shall be numeric or _logical_, rank 1.

- **vector_b**
  : The type shall be numeric if vector_a is of numeric type or _logical_
  if vector_a is of type _logical_. vector_b shall be a rank-one
  array.

### **Result**

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is _.true._ or _.false._.

### **Examples**

Sample program:

```fortran
program demo_dot_prod
implicit none
    integer, dimension(3) :: a, b
    a = [ 1, 2, 3 ]
    b = [ 4, 5, 6 ]
    print '(3i3)', a
    print *
    print '(3i3)', b
    print *
    print *, dot_product(a,b)
end program demo_dot_prod
```
Results:
```text
     1  2  3

     4  5  6

             32
```
### **Standard**

Fortran 95

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## dprod

### **Name**

**dprod**(3) - \[NUMERIC\] Double precision real product

### **Synopsis**
```fortran
    result = dprod(x,y)
```
```fortran
     elemental function dprod(x,y)

      real,intent(in) :: x
      real,intent(in) :: y
      doubleprecision :: dprod
```
### **Characteristics**

**x** and **y** must both be real values of default kind.

The return value is doubleprecision (ie. _real(kind=kind(0.0d0))_).

The setting of compiler options specifying the size of a default _real_
can affect this function.

### **Description**

**dprod**(3) produces a _doubleprecision_ product of default _real_
values **x** and **y**.

That is, it is expected to convert the arguments to double precision
before multiplying, which a simple expression **x\*y** would not be
required to do. This can be significant in specialized computations
requiring high precision.

The result has a value equal to a processor-dependent approximation to
the product of **x** and **y**. Note it is recommended in the standard
that the processor compute the product in double precision, rather than
in single precision then converted to double precision; but is only
a recommendation.

### **Options**

- **x**
  : the multiplier

- **y**
  : the multiplicand

### **Result**

The returned value of the product should have the same value as
**dble(x)\*dble(y)**.

### **Examples**

Sample program:

```fortran
program demo_dprod
implicit none
integer,parameter :: dp=kind(0.0d0)
real :: x = 5.2
real :: y = 2.3
doubleprecision :: xx
real(kind=dp)   :: dd

   print *,'algebraically 5.2 x 2.3 is exactly 11.96'
   print *,'as floating point values results may differ slightly:'
   ! basic usage
   dd = dprod(x,y)
   print *, 'compare dprod(xy)=',dd, &
   & 'to x*y=',x*y, &
   & 'to dble(x)*dble(y)=',dble(x)*dble(y)

   print *,'test if an expected result is produced'
   xx=-6.0d0
   write(*,*)DPROD(-3.0, 2.0),xx
   write(*,*)merge('PASSED','FAILED',DPROD(-3.0, 2.0) == xx)

   print *,'elemental'
   print *, dprod( [2.3,3.4,4.5], 10.0 )
   print *, dprod( [2.3,3.4,4.5], [9.8,7.6,5.4] )

end program demo_dprod
```
Results:
(this can vary between programming environments):
```text
    algebraically 5.2 x 2.3 is exactly 11.96
    as floating point values results may differ slightly:
    compare dprod(xy)=   11.959999313354501      to x*y=   11.9599991
    to dble(x)*dble(y)=   11.959999313354501
    test if an expected result is produced
     -6.0000000000000000       -6.0000000000000000
    PASSED
    elemental
      22.999999523162842        34.000000953674316        45.000000000000000
      22.539999971389761        25.840000400543204        24.300000429153442
```
### **Standard**

FORTRAN 77

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## dshiftl

### **Name**

**dshiftl**(3) - \[BIT:COPY\] Combined left shift of the bits of two integers

### **Synopsis**
```fortran
    result = dshiftl(i, j, shift)
```
```fortran
     elemental integer(kind=KIND) function dshiftl(i, j, shift)

      integer(kind=KIND),intent(in) :: i
      integer(kind=KIND),intent(in) :: j
      integer(kind=**),intent(in) :: shift
```
### **Characteristics**

  - the kind of **i**, **j**, and the return value are the same. An
    exception is that one of **i** and **j** may be a BOZ literal constant
    (A BOZ literal constant is a binary, octal or hex constant).

  - If either I or J is a BOZ-literal-constant (but not both), it is
    first converted as if by the intrinsic function **int**(3) to type
    _integer_ with the kind type parameter of the other.

  - a kind designated as ** may be any supported kind value for the type

### **Description**

  **dshiftl**(3) combines bits of **i** and **j**. The rightmost **shift**
  bits of the result are the leftmost **shift** bits of **j**, and the
  remaining bits are the rightmost **bitsize(i)-shift** of **i**.

  Hence **dshiftl** is designated as a "combined left shift", because
  it is like we appended **i** and **j** together, shifted it **shift**
  bits to the left, and then kept the same number of bits as **i** or
  **j** had.

  For example, for two 16-bit values if **shift=6**
```text
      SHIFT=6
      I =             1111111111111111
      J =             0000000000000000
      COMBINED        11111111111111110000000000000000
      DROP LEFT BITS  11111111110000000000000000
      KEEP LEFT 16    1111111111000000
```
#### NOTE
  This is equivalent to
```fortran
     ior( shiftl(i, shift), shiftr(j, bit_size(j) - shift) )
```
  Also note that using this last representation of the operation is can
  be derived that when both **i** and **j** have the same value as in
```fortran
      dshiftl(i, i, shift)
```
  the result has the same value as a circular shift:
```fortran
      ishftc(i, shift)
```
### **Options**

- **i**
  : used to define the left pattern of bits in the combined pattern

- **j**
  : used for the right pattern of bits in the combined pattern

- **shift**
  : shall be nonnegative and less than or equal to the number of bits
    in an _integer_ input value (ie. the bit size of either one that is
    not a BOZ literal constant).

### **Result**

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
   j=int(b"11111000000000000000000000000000")
   ! and the other bits are the rightmost bits of I
   i=int(b"00000000000000000000000000000000")
   call printit()

   j=int(b"11111000000000000000000000000000")
   i=int(b"00000111111111111111111111111111")
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

Fortran 2008

### **See Also**

[**dshiftr**(3)](#dshiftr)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## dshiftr

### **Name**

**dshiftr**(3) - \[BIT:COPY\] Combined right shift of the bits of two integers

### **Synopsis**
```fortran
    result = dshiftr(i, j, shift)
```
```fortran
     elemental integer(kind=KIND) function dshiftr(i, j, shift)

      integer(kind=KIND),intent(in) :: i
      integer(kind=KIND),intent(in) :: j
      integer(kind=**),intent(in) :: shift
```
### **Characteristics**

  - a kind designated as ** may be any kind value for the _integer_ type

  - the kind of **i**, **j**, and the return value are the same. An
    exception is that one of **i** and **j** may be a BOZ literal constant
    (A BOZ literal constant is a binary, octal or hex constant).

  - If either I or J is a BOZ-literal-constant, it is first converted
    as if by the intrinsic function **int**(3) to type _integer_ with the
    kind type parameter of the other.

### **Description**

  **dshiftr**(3) combines bits of **i** and **j**. The leftmost **shift**
  bits of the result are the rightmost **shift** bits of **i**, and the
  remaining bits are the leftmost bits of **j**.

  It may be thought of as appending the bits of **i** and **j**, dropping
  off the **shift** rightmost bits, and then retaining the same number
  of rightmost bits as an input value, hence the name "combined right
  shift"...

Given two 16-bit values labeled alphabetically ...
```text
   i=ABCDEFGHIJKLMNOP
   j=abcdefghijklmnop
```
Append them together
```text
   ABCDEFGHIJKLMNOPabcdefghijklmnop
```
Shift them N=6 bits to the right dropping off bits
```text
         ABCDEFGHIJKLMNOPabcdefghij
```
Keep the 16 right-most bits
```text
                   KLMNOPabcdefghij
```
#### NOTE

**dshifr(i,j,shift)** is equivalent to
```fortran
     ior(shiftl (i, bit_size(i) - shift), shiftr(j, shift) )
```
it can also be seen that if **i** and **j** have the same
value
```fortran
     dshiftr( i, i, shift )
```
this has the same result as a negative circular shift
```fortran
     ishftc( i,   -shift ).
```
### **Options**

- **i**
  : left value of the pair of values to be combine-shifted right

- **j**
  : right value of the pair of values to be combine-shifted right

- **shift**
  : the shift value is non-negative and less than or equal to the number
    of bits in an input value as can be computed by **bit_size**(3).

### **Result**

The result is a combined right shift of **i** and **j** that is the
same as the bit patterns of the inputs being combined left to right,
dropping off **shift** bits on the right and then retaining the same
number of bits as an input value from the rightmost bits.

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

   ! print values
    write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
    write(*,'(b32.32)') i,j, dshiftr (i, j, shift)

  ! visualizing a "combined right shift" ...
   i=int(b"00000000000000000000000000011111")
   j=int(b"11111111111111111111111111100000")
   ! appended together ( i//j )
   ! 0000000000000000000000000001111111111111111111111111111111100000
   ! shifted right SHIFT values dropping off shifted values
   !      00000000000000000000000000011111111111111111111111111111111
   ! keep enough rightmost bits to fill the kind
   !                                 11111111111111111111111111111111
   ! so the result should be all 1s bits ...

    write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
    write(*,'(b32.32)') i,j, dshiftr (i, j, shift)

end program demo_dshiftr
```
Results:
```text
     1342177280
   I=-1 J=0 SHIFT=5
   11111111111111111111111111111111
   00000000000000000000000000000000
   11111000000000000000000000000000
   I=31 J=-32 SHIFT=5
   00000000000000000000000000011111
   11111111111111111111111111100000
   11111111111111111111111111111111
```
### **Standard**

Fortran 2008

### **See Also**

[**dshiftl**(3)](#dshiftl)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## eoshift

### **Name**

**eoshift**(3) - \[TRANSFORMATIONAL\] End-off shift elements of an array

### **Synopsis**
```fortran
  result = eoshift( array, shift [,boundary] [,dim] )
```
```fortran
   type(TYPE(kind=KIND)) function eoshift(array,shift,boundary,dim)

    type(TYPE(kind=KIND)),intent(in) :: array(..)
    integer(kind=**),intent(in)      :: shift
    type(TYPE(kind=KIND)),intent(in) :: boundary
    integer(kind=**),intent(in)      :: dim
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

 - **array** May be any type, but not a scalar.

 - **shift** is an integer of any dind

 - **boundary** is a scalar of the same type and kind as the **array**.

 - **dim** is an integer of any dind

 - The result is an array of same type, kind and rank as the **array** argument.

### **Description**

  **eoshift**(3) performs an end-off shift on elements of **array**
  along the dimension of **dim**.

  Elements shifted out one end of each rank one section are dropped.

  If **boundary** is present then the corresponding value from
  **boundary** is copied back in the other end, else default values
  are used.

### **Options**

- **array**
  : array of any type whose elements are to be shifted.
  If the rank of **array** is one, then all elements of **array** are
  shifted by **shift** places. If rank is greater than one, then all
  complete rank one sections of **array** along the given dimension
  are shifted.

- **shift**
  : the number of elements to shift

- **boundary**
  : the value to use to fill in the elements vacated by the shift.
  If **boundary** is not present then the following are copied in
  depending on the type of **array**.
```text
    Array Type     | Boundary Value
    -----------------------------------------------------
    Numeric        | 0 of the type and kind of "array"
    Logical        | .false.
    Character(len) |  LEN blanks
```
- **dim**
  :  **dim** is in the range of
```fortran
    1 <= DIM <= n
```
  where **"n"** is the rank of **array**.  If **dim** is omitted it
  is taken to be **1**.

### **Result**

 Returns an array of the same characteristics as the input with the
 specified number of elements dropped off along the specified direction
 indicated, backfilling the vacated elements with a value indicated by
 the **boundary** value.

### **Examples**

Sample program:

```fortran
program demo_eoshift
implicit none
integer, dimension(3,3) :: a
integer :: i

    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', (a(i,:),i=1,3)

    print *

    ! shift it
    a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
    print '(3i3)', (a(i,:),i=1,3)

end program demo_eoshift
```
Results:

```text
     1  4  7
     2  5  8
     3  6  9

     4  7 -5
     8 -5 -5
     6  9 -5
```
### **Standard**

Fortran 95

### **See Also**

[**dshiftr**(3)](#dshiftr),
[**dshiftl**(3)](#dshiftl)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## epsilon

### **Name**

**epsilon**(3) - \[NUMERIC MODEL\] Epsilon function

### **Synopsis**
```fortran
    result = epsilon(x)
```
```fortran
     real(kind=kind(x)) function epsilon(x)

      real(kind=kind(x),intent(in)   :: x
```
### **Characteristics**

### **Description**

**epsilon**(3) returns the floating point relative accuracy.
It is the nearly negligible number relative to **1**
such that **1+ little_number** is not equal to **1**; or more
precisely
```fortran
   real( 1.0, kind(x)) + epsilon(x) /=  real( 1.0, kind(x))
```
It may be thought of as the distance from 1.0 to the next largest
floating point number.

One use of **epsilon**(3) is to select a _delta_ value for algorithms that
search until the calculation is within _delta_ of an estimate.

If _delta_ is too small the algorithm might never halt, as a computation
summing values smaller than the decimal resolution of the data type does
not change.

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of the same type as the argument.

### **Examples**

Sample program:

```fortran
program demo_epsilon
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x = 3.143
real(kind=dp) :: y = 2.33d0

   ! so if x is of type real32, epsilon(x) has the value 2**-23
   print *, epsilon(x)
   ! note just the type and kind of x matter, not the value
   print *, epsilon(huge(x))
   print *, epsilon(tiny(x))

   ! the value changes with the kind of the real value though
   print *, epsilon(y)

   ! adding and subtracting epsilon(x) changes x
   write(*,*)x == x + epsilon(x)
   write(*,*)x == x - epsilon(x)

   ! these next two comparisons will be .true. !
   write(*,*)x == x + epsilon(x) * 0.999999
   write(*,*)x == x - epsilon(x) * 0.999999

   ! you can calculate epsilon(1.0d0)
   write(*,*)my_dp_eps()

contains

   function my_dp_eps()
   ! calculate the epsilon value of a machine the hard way
   real(kind=dp) :: t
   real(kind=dp) :: my_dp_eps

      ! starting with a value of 1, keep dividing the value
      ! by 2 until no change is detected. Note that with
      ! infinite precision this would be an infinite loop,
      ! but floating point values in Fortran have a defined
      ! and limited precision.
      my_dp_eps = 1.0d0
      SET_ST: do
         my_dp_eps = my_dp_eps/2.0d0
         t = 1.0d0 + my_dp_eps
         if (t <= 1.0d0) exit
      enddo SET_ST
      my_dp_eps = 2.0d0*my_dp_eps

   end function my_dp_eps
end program demo_epsilon
```
Results:
```text
  1.1920929E-07
  1.1920929E-07
  1.1920929E-07
  2.220446049250313E-016
 F
 F
 T
 T
  2.220446049250313E-016
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
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

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## erfc

### **Name**

**erfc**(3) - \[MATHEMATICS\] Complementary error function

### **Synopsis**
```fortran
    result = erfc(x)
```
```fortran
     elemental real(kind=KIND) function erfc(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

### **Description**

**erfc**(3) computes the complementary error function of **x**. Simply put
this is equivalent to **1 - erf(x)**, but **erfc** is provided because
of the extreme loss of relative accuracy if **erf(x)** is called for
large **x** and the result is subtracted from **1**.

**erfc(x)** is defined as

<!--
$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_0^x e^{-t^2} dt.
$$
-->

$$
\text{erfc}(x) = 1 - \text{erf}(x) = 1 - \frac{2}{\sqrt{\pi}} \int_x^{\infty} e^{-t^2} dt.
$$

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of type _real_ and of the same kind as **x**. It lies in
the range

> 0 \<= **erfc**(x) \<= 2.

### **Examples**

Sample program:

```fortran
program demo_erfc
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erfc(x)
end program demo_erfc
```

Results:

```text
     0.17000000000000001       0.81000753879819121
```

### **Standard**

Fortran 2008

### ***See also**

[**erf**(3)](#erf)
[**erf_scaled**(3)](#erf_scaled)

### **Resources**

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## erfc_scaled

### **Name**

**erfc_scaled**(3) - \[MATHEMATICS\] Error function

### **Synopsis**
```fortran
    result = erfc_scaled(x)
```
```fortran
     elemental real(kind=KIND) function erfc_scaled(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

### **Description**

**erfc_scaled**(3) computes the exponentially-scaled complementary
error function of **x**:

$$
e^{x^2} \frac{2}{\sqrt{\pi}} \int_{x}^{\infty}
e^{-t^2} dt.
$$

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of type _real_ and of the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_erfc_scaled
implicit none
real(kind(0.0d0)) :: x = 0.17d0
   x = erfc_scaled(x)
   print *, x
end program demo_erfc_scaled
```
Results:

```text
     0.83375830214998126
```
### **Standard**

Fortran 2008

### **See also**

[**erf**(3)](#erf)
[**erfc**(3)](#erfc)

 _fortran-lang intrinsic descriptions_

## erf

### **Name**

**erf**(3) - \[MATHEMATICS\] Error function

### **Synopsis**
```fortran
    result = erf(x)
```
```fortran
     elemental real(kind=KIND) function erf(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

- The result is of the same _type_ and _kind_ as **x**.

### **Description**

**erf**(3) computes the error function of **x**, defined as

$$
\text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{__-t__^2} dt.
$$

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of type _real_, of the same kind as **x** and lies in the
range **-1** \<= **erf**(x) \<= 1 .

### **Examples**

Sample program:

```fortran
program demo_erf
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 0.17_real64
    write(*,*)x, erf(x)
end program demo_erf
```

Results:

```text
     0.17000000000000001       0.18999246120180879
```

### **Standard**

Fortran 2008

### **See also**

[**erfc**(3)](#erfc),
[**erf_scaled**(3)](#erfc_scaled)

### **Resources**

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

 _fortran-lang intrinsic descriptions_

## event_query

### **Name**

**event_query**(3) - \[COLLECTIVE\] Query whether a coarray event has occurred

### **Synopsis**
```fortran
    call event_query(event, count [,stat] )
```
```fortran
```
### **Characteristics**

### **Description**

**event_query**(3) assigns the number of events to **count** which have been
posted to the **event** variable and not yet been removed by calling
**event_wait**. When **stat** is present and the invocation was successful, it
is assigned the value **0**. If it is present and the invocation has failed,
it is assigned a positive value and **count** is assigned the value **-1**.

### **Options**

- **event**
  : (intent(in)) Scalar of type event_type, defined in
  iso_fortran_env; shall not be coindexed.

- **count**
  : (intent(out))Scalar integer with at least the precision of default
  _integer_.

- **stat**
  : (OPTIONAL) Scalar default-kind _integer_ variable.

### **Examples**

Sample program:

```fortran
program demo_event_query
use iso_fortran_env
implicit none
type(event_type) :: event_value_has_been_set[*]
integer :: cnt
   if (this_image() == 1) then
      call event_query(event_value_has_been_set, cnt)
      if (cnt > 0) write(*,*) "Value has been set"
   elseif (this_image() == 2) then
      event post(event_value_has_been_set[1])
   endif
end program demo_event_query
```
### **Standard**

TS 18508

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## execute_command_line

### **Name**

**execute_command_line**(3) - \[SYSTEM:PROCESSES\] Execute a shell command

### **Synopsis**
```fortran
    call execute_command_line( &
    & command [,wait] [,exitstat] [,cmdstat] [,cmdmsg] )
```
```fortran
     subroutine execute_command_line(command,wait,exitstat,cmdstat,cmdmsg)

      character(len=*),intent(in)  :: command
      logical,intent(in),optional  :: wait
      integer,intent(out),optional :: exitstat
      integer,intent(out),optional :: cmdstat
      character(len=*),intent(out),optional :: cmdmsg
```
### **Characteristics**

### **Description**

For **execute_command_line**(3) the **command** argument is passed to the
shell and executed. (The shell is generally **sh**(1) on Unix systems, and
cmd.exe on Windows.) If **wait** is present and has the value _.false._,
the execution of the command is asynchronous if the system supports it;
otherwise, the command is executed synchronously.

The three last arguments allow the user to get status information. After
synchronous execution, **exitstat** contains the integer exit code of the
command, as returned by **system**. **cmdstat** is set to zero if the command
line was executed (whatever its exit status was). **cmdmsg** is assigned an
error message if an error has occurred.

Note that the system call need not be thread-safe. It is the
responsibility of the user to ensure that the system is not called
concurrently if required.

When the command is executed synchronously, **execute_command_line**
returns after the command line has completed execution. Otherwise,
**execute_command_line** returns without waiting.

### **Options**

- **command**
  : a default _character_ scalar containing the command line to be
  executed. The interpretation is programming-environment dependent.

- **wait**
  : (Optional) a default _logical_ scalar. If **wait** is present with the
  value .false., and the processor supports asynchronous execution of
  the command, the command is executed asynchronously; otherwise it is
  executed synchronously.

- **exitstat**
  : (Optional) an _integer_ of the default kind with **intent(inout)**. If
  the command is executed synchronously, it is assigned the value of
  the processor-dependent exit status. Otherwise, the value of
  **exitstat** is unchanged.

- **cmdstat**
  : (Optional) an _integer_ of default kind with **intent(inout)**. If an
  error condition occurs and **cmdstat** is not present, error termination
  of execution of the image is initiated.

  It is assigned the value **-1** if the processor does not support
  command line execution, a processor-dependent positive value if an
  error condition occurs, or the value **-2** if no error condition
  occurs but **wait** is present with the value false and the processor
  does not support asynchronous execution. Otherwise it is assigned
  the value 0.

- **cmdmsg**
  : (Optional) a _character_ scalar of the default kind. It is an **intent
  (inout)** argument.If an error condition occurs, it is assigned a
  processor-dependent explanatory message.Otherwise, it is unchanged.

### **Examples**

Sample program:

```fortran
program demo_exec
implicit none
   integer :: i

   call execute_command_line("external_prog.exe", exitstat=i)
   print *, "Exit status of external_prog.exe was ", i

   call execute_command_line("reindex_files.exe", wait=.false.)
   print *, "Now reindexing files in the background"
end program demo_exec
```
### **Note**

Because this intrinsic is making a system call, it is very system
dependent. Its behavior with respect to signaling is processor
dependent. In particular, on POSIX-compliant systems, the SIGINT and
SIGQUIT signals will be ignored, and the SIGCHLD will be blocked. As
such, if the parent process is terminated, the child process might not
be terminated alongside.

### **Standard**

Fortran 2008

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## exp

### **Name**

**exp**(3) - \[MATHEMATICS\] Base-e exponential function

### **Synopsis**
```fortran
    result = exp(x)
```
```fortran
     elemental TYPE(kind=KIND) function exp(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** may be _real_ or _complex_.
 - The return value has the same type and kind as **x**.

### **Description**

**exp**(3) returns the value of _e_ (the base of natural logarithms)
raised to the power of **x**.

"_e_" is also known as _Euler's constant_.

If **x** is of type _complex_, its imaginary part is regarded as a value
in radians such that if (see _Euler's formula_):
```fortran
    cx=(re,im)
```
then
```fortran
    exp(cx) = exp(re) * cmplx(cos(im),sin(im),kind=kind(cx))
```
Since **exp**(3) is the inverse function of **log**(3) the maximum valid magnitude
of the _real_ component of **x** is **log(huge(x))**.

### **Options**

- **x**
  : The type shall be _real_ or _complex_.

### **Result**

The value of the result is **e\*\*x** where **e** is Euler's constant.

### **Examples**

Sample program:

```fortran
program demo_exp
implicit none
real :: x, re, im
complex :: cx

   x = 1.0
   write(*,*)"Euler's constant is approximately",exp(x)

   !! complex values
   ! given
   re=3.0
   im=4.0
   cx=cmplx(re,im)

   ! complex results from complex arguments are Related to Euler's formula
   write(*,*)'given the complex value ',cx
   write(*,*)'exp(x) is',exp(cx)
   write(*,*)'is the same as',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))

   ! exp(3) is the inverse function of log(3) so
   ! the real component of the input must be less than or equal to
   write(*,*)'maximum real component',log(huge(0.0))
   ! or for double precision
   write(*,*)'maximum doubleprecision component',log(huge(0.0d0))

   ! but since the imaginary component is passed to the cos(3) and sin(3)
   ! functions the imaginary component can be any real value

end program demo_exp
```

Results:

```text
 Euler's constant is approximately   2.718282
 given the complex value  (3.000000,4.000000)
 exp(x) is (-13.12878,-15.20078)
 is the same as (-13.12878,-15.20078)
 maximum real component   88.72284
 maximum doubleprecision component   709.782712893384
```

### **Standard**

FORTRAN 77

### **See Also**

- [**log**(3)](#log)

### **Resources**

- Wikipedia:[Exponential function](https://en.wikipedia.org/wiki/Exponential_function)

- Wikipedia:[Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## exponent

### **Name**

**exponent**(3) - \[MODEL_COMPONENTS\] Exponent function

### **Synopsis**
```fortran
    result = exponent(x)
```
```fortran
     elemental integer function exponent(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

### **Description**

**exponent**(3) returns the value of the exponent part of **x**. If **x** is
zero the value returned is zero.

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is of type default _integer_.

### **Examples**

Sample program:

```fortran
program demo_exponent
implicit none
real :: x = 1.0
integer :: i
   i = exponent(x)
   print *, i
   print *, exponent(0.0)
end program demo_exponent
```

Results:

```text
              1
              0
```

### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
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

 _fortran-lang intrinsic descriptions_

## extends_type_of

### **Name**

**extends_type_of**(3) - \[STATE\] Determine if the dynamic type of **a** is an extension of the dynamic type of **mold**.

### **Synopsis**
```fortran
    result = extends_type_of(a, mold)
```
```fortran
     logical extends_type_of(a, mold)

      type(TYPE(kind=KIND),intent(in) :: a
      type(TYPE(kind=KIND),intent(in) :: mold
```
### **Characteristics**

**a** an object of any extensible type; it may also be a pointer
to an object extensible type.

**mold** an object of any extensible type; it may also be a pointer
to an object extensible type.

**description**

  **extends_type_of**(3) is .true. if and only if the dynamic type of
  **a** is or could be (for unlimited polymorphic) an extension of the
  dynamic type of **mold**.

**options**
- **a**
    : be an object of extensible declared type or unlimited
    polymorphic. If it is a polymorphic pointer, it shall not have an
    undefined association status.

- **mold**
    : be an object of extensible declared type or unlimited
    polymorphic. If it is a polymorphic pointer, it shall not have an
    undefined association status.

### **Result**

  If **mold** is unlimited polymorphic and is either a disassociated
  pointer or unallocated allocatable variable, the result is true.

  Otherwise if **a** is unlimited polymorphic and is either a
  disassociated pointer or unallocated allocatable variable, the result
  is false.

  Otherwise the result is true if and only if the dynamic type of **a**

  if the dynamic type of A or MOLD is extensible, the result is true if
  and only if the dynamic type of A is an extension type of the dynamic
  type of MOLD; otherwise the result is processor dependent.

  NOTE1

  The dynamic type of a disassociated pointer or unallocated allocatable
  variable is its declared type.

  NOTE2

  The test performed by **extends_type_of** is not the same as the
  test performed by the type guard **class is**. The test performed by
  **extends_type_of** does not consider kind type parameters.

### **Examples**

Sample program:
```fortran
  ! program demo_extends_type_of
  module M_demo_extends_type_of

  type nothing
  end type nothing

  type, extends(nothing) :: dot
    real :: x=0
    real :: y=0
  end type dot

  type, extends(dot) :: point
    real :: z=0
  end type point

  type something_else
  end type something_else

  end module M_demo_extends_type_of

  program demo_extends_type_of
  use M_demo_extends_type_of, only : nothing, dot, point, something_else
  type(nothing) :: grandpa
  type(dot) :: dad
  type(point) :: me
  type(something_else) :: alien

   write(*,*)'these should all be true'
   write(*,*)extends_type_of(me,grandpa),'I am descended from Grandpa'
   write(*,*)extends_type_of(dad,grandpa),'Dad is descended from Grandpa'
   write(*,*)extends_type_of(me,dad),'Dad is my ancestor'

   write(*,*)'is an object an extension of itself?'
   write(*,*)extends_type_of(grandpa,grandpa) ,'self-propogating!'
   write(*,*)extends_type_of(dad,dad) ,'clone!'

   write(*,*)' you did not father your grandfather'
   write(*,*)extends_type_of(grandpa,dad),'no paradox here'

   write(*,*)extends_type_of(dad,me),'no paradox here'
   write(*,*)extends_type_of(grandpa,me),'no relation whatsoever'
   write(*,*)extends_type_of(grandpa,alien),'no relation'
   write(*,*)extends_type_of(me,alien),'not what everyone thinks'

   call pointers()
   contains

   subroutine pointers()
   ! Given the declarations and assignments
   type t1
   real c
   end type
   type, extends(t1) :: t2
   end type
   class(t1), pointer :: p, q
      allocate (p)
      allocate (t2 :: q)
      ! the result of EXTENDS_TYPE_OF (P, Q) will be false, and the result
      ! of EXTENDS_TYPE_OF (Q, P) will be true.
      write(*,*)'(P,Q)',extends_type_of(p,q),"mind your P's and Q's"
      write(*,*)'(Q,P)',extends_type_of(q,p)
   end subroutine pointers

  end program demo_extends_type_of
```
Results:
```text
    these should all be true
    T I am descended from Grandpa
    T Dad is descended from Grandpa
    T Dad is my ancestor
    is an object an extension of itself?
    T self-propogating!
    T clone!
     you did not father your grandfather
    F no paradox here
    F no paradox here
    F no relation whatsoever
    F no relation
    F not what everyone thinks
    (P,Q) F mind your P's and Q's
    (Q,P) T
```
### **Standard**

   Fortran 2003

### **See Also**

[**same_type_as**(3)](#same_type_as)

  _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## findloc

### **Name**

**findloc**(3) - \[ARRAY:LOCATION\] Location of first element of ARRAY
identified by MASK along dimension DIM matching a target value

### **Synopsis**

```fortran
    result = findloc (array, value, dim [,mask] [,kind] [,back])
```
```fortran
     function findloc (array, value, dim, mask, kind, back)

      integer(kind=KIND),  intent(in)      :: array(..)
      integer(kind=KIND),  intent(in)      :: value
      integer(kind=KIND),  intent(in)      :: dim
      logical(kind=**),intent(in),optional :: mask(..)
      integer(kind=**),intent(in),optional :: kind
      logical(kind=**),intent(in),optional :: back
```
or
```fortran
    result = findloc(array, value [,mask] [,kind] [,back])
```
```fortran
     function findloc (array, value, mask, kind, back)

      integer(kind=KIND),  intent(in)       :: array(..)
      integer(kind=KIND),  intent(in)       :: value
      logical(kind=**), intent(in),optional :: mask(..)
      integer(kind=**), intent(in),optional :: kind
      logical(kind=**),intent(in),optional  :: back
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **array** shall be an array of intrinsic type.
- **value** shall be scalar but in type conformance with **array**
- **dim** The corresponding actual argument shall not be an optional dummy argument.
- **mask** shall be conformable with **array**.
- **kind** a scalar integer initialization expression (ie. a constant)
- **back** a logical scalar.

### **Description**

**findloc**(3) returns the location of the first element of **array**
identified by **mask** along dimension **dim** having a value equal
to **value**.

If both **array** and **value** are of type logical, the comparison is
performed with the **.eqv.** operator; otherwise, the comparison is
performed with the == operator. If the value of the comparison is
_.true._, that element of **array** matches **value**.

If only one element matches **value**, that element's subscripts are
returned. Otherwise, if more than one element matches **value** and
**back** is absent or present with the value _.false._, the element whose
subscripts are returned is the first such element, taken in array
element order. If **back** is present with the value _.true._, the element
whose subscripts are returned is the last such element, taken in array
element order.

### **Options**

- **array**
  : shall be an array of intrinsic type.

- **value**
  : shall be scalar and in type conformance with **array**, as specified
  in Table 7.3 for relational intrinsic operations 7.1.5.5.2).

- **dim**
  : shall be an integer scalar with a value in the range 1 **DIM** n, where
  n is the rank of **array**. The corresponding actual argument shall
  not be an optional dummy argument.

- **mask**
  : (optional) shall be of type logical and shall be conformable with
  **array**.

- **kind**
  : (optional) shall be a scalar integer initialization expression.

- **back**
  : (optional) shall be a logical scalar.

### **Result**

Result Characteristics. Integer. If **kind** is present, the kind type
parameter is that specified by the value of **kind**; otherwise the kind
type parameter is that of default integer type. If **dim** does not appear,
the result is an array of rank one and of size equal to the rank of
**array**; otherwise, the result is of rank n - 1 and shape
```
   [d1, d2, . . ., dDIM-1, dDIM+1, . . ., dn ]
```
where
```
   [d1, d2, . . ., dn ]
```
is the shape of **array**.

### **Result**

- **Case (i):**
  The result of **findloc (array, value)** is a rank-one array whose
  element values are the values of the subscripts of an element of
  **array** whose value matches **value**. If there is such a value, the
  ith subscript returned lies in the range 1 to ei, where ei is the
  extent of the ith dimension of **array**. If no elements match **value**
  or **array** has size zero, all elements of the result are zero.

- **Case (ii):**
  the result of **findloc (array, value, mask = mask)** is a
  rank-one array whose element values are the values of the subscripts
  of an element of **array**, corresponding to a true element of **mask**,
  whose value matches **value**. If there is such a value, the ith
  subscript returned lies in the range 1 to ei, where ei is the
  extent of the ith dimension of **array**. If no elements match
  **value**, **array** has size zero, or every element of **mask** has the
  value false, all elements of the result are zero.

- **Case (iii):**
  If **array** has rank one, the result of
```
      findloc (array, value, dim=dim [, mask = mask])
```
is a scalar whose value is equal to that of the first element of
```
      findloc (array, value [, mask = mask])
```
Otherwise, the value of element
```
      (s1, s2, . . ., sDIM-1, sDIM+1, . . ., sn )
```
of the result is equal to
```
      findloc (array (s1, s2, ..., sdim-1, :, sdim+1, ..., sn ), &
      value, dim=1 [, mask = mask (s1, s2, ..., sdim-1, :,
                      sdim+1, ..., sn )]).
```
### **Examples**

- **Case (i):**
  The value of
```
        findloc ([2, 6, 4, 6,], value = 6)
```
is \[2\], and the value of
```
        findloc ([2, 6, 4, 6], value = 6, back = .true.)
```
is \[4\].

- **Case (ii):**
  If **a** has the value
```text
      0 -5  7 7
      3  4 -1 2
      1  5  6 7
```
and **m** has the value
```text
       T T F T
       T T F T
       T T F T

      findloc (a, 7, mask = m)
```
has the value \[1, 4\] and
```
      findloc (a, 7, mask = m, back = .true.)
```
has the value \[3, 4\]. This is independent of the declared lower
bounds for **a** .

- **Case (iii):**
  The value of
```
      findloc ([2, 6, 4], value = 6, dim = 1)
```
is 2. If **b** has the value
```
       1 2 -9
       2 2  6
```
> findloc (b, **value** = 2, dim = 1)

has the value \[2, 1, 0\] and
```
      findloc (b, value = 2, dim = 2)
```
has the value \[2, 1\]. This is independent of the declared lower
bounds for **b**.

 _fortran-lang intrinsic descriptions_

## floor

### **Name**

**floor**(3) - \[NUMERIC\] Function to return largest integral value
not greater than argument

### **Synopsis**
```fortran
    result = floor(a [,kind])
```
```fortran
     elemental integer(kind=kind) function floor( a ,kind )

      real(kind=KIND),intent(in) :: a
      integer,intent(in),optional :: kind
```
### **Characteristics**

where _KIND_ is any valid value for type _integer_.

### **Description**

**floor**(3) returns the greatest integer less than or equal to **a**.
That is, it picks the whole number at or to the left of the value on
the scale **-huge(int(a,kind=KIND))-1** to **huge(int(a),kind=KIND)**.

### **Options**

- **a**
  : The type shall be _real_.

- **kind**
  : (Optional) A scalar _integer_ constant initialization expression
  indicating the kind parameter of the result.

### **Result**

The return value is of type _integer(kind)_ if **kind** is present and of
default-kind _integer_ otherwise.

The result is undefined if it cannot be represented in the specified
integer type.

### **Examples**

Sample program:

```fortran
program demo_floor
implicit none
real :: x = 63.29
real :: y = -63.59
    print *, x, floor(x)
    print *, y, floor(y)
   ! elemental
   print *,floor([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

   ! note even a small deviation from the whole number changes the result
   print *,      [2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)]
   print *,floor([2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)])

   ! A=Nan, Infinity or  <huge(0_KIND)-1 < A > huge(0_KIND) is undefined
end program demo_floor
```

Results:

```text
      63.29000              63
     -63.59000             -64
             -3          -3          -3          -2          -2          -1
             -1           0           0           1           1           2
              2           2           2
      2.000000       2.000000       2.000000
              2           1           1
```

### **Standard**

Fortran 95

### **See Also**

[**ceiling**(3)](#ceiling),
[**nint**(3)](#nint)

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**int**(3)](#int),
[**selected_int_kind**(3)](#selected_int_kind)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## fraction

### **Name**

**fraction**(3) - \[MODEL_COMPONENTS\] Fractional part of the model representation

### **Synopsis**
```fortran
    result = fraction(x)
```
```fortran
     elemental real(kind=KIND) function fraction(x)

      real(kind=KIND),intent(in) :: fraction
```
### **Characteristics**

  The result has the same characteristics as the argument.

### **Description**

  **fraction**(3) returns the fractional part of the model representation
  of **x**.

### **Options**

- **x**
  : The value to interrogate

### **Result**

The fractional part of the model representation of **x** is returned;
it is **x \* radix(x)\*\*(-exponent(x))**.

If **x** has the value zero, the result is zero.

If **x** is an IEEE NaN, the result is that NaN.

If **x** is an IEEE infinity, the result is an IEEE NaN.

### **Examples**

Sample program:

```fortran
program demo_fraction
implicit none
real :: x
   x = 178.1387e-4
   print *, fraction(x), x * radix(x)**(-exponent(x))
end program demo_fraction
```
Results:

```text
     0.570043862      0.570043862
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**huge**(3)](#huge),
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

 _fortran-lang intrinsic descriptions_

## gamma

### **Name**

**gamma**(3) - \[MATHEMATICS\] Gamma function, which yields factorials for positive whole numbers

### **Synopsis**
```fortran
    result = gamma(x)
```
```fortran
     elemental real(kind=KIND) function gamma( x)

      type(real,kind=KIND),intent(in) :: x
```
### **Characteristics**

- **gamma**(3) returns a _real_ value with the kind as **x**.

### **Description**

**gamma(x)** computes Gamma of **x**. For positive whole number values of **n** the
Gamma function can be used to calculate factorials, as **(n-1)! == gamma(real(n))**.
That is
```text
n! == gamma(real(n+1))
```
$$
\\__Gamma__(x) = \\int\_0\*\*\\infty
t\*\*{x-1}{\\mathrm{e}}\*\*{__-t__}\\,{\\mathrm{d}}t
$$

### **Options**

- **x**
  : Shall be of type _real_ and neither zero nor a negative integer.

### **Result**

The return value is of type _real_ of the same kind as _x_.

### **Examples**

Sample program:

```fortran
program demo_gamma
use, intrinsic :: iso_fortran_env, only : wp=>real64
implicit none
real :: x, xa(4)
integer :: i

   x = gamma(1.0)
   write(*,*)'gamma(1.0)=',x

   ! elemental
   xa=gamma([1.0,2.0,3.0,4.0])
   write(*,*)xa
   write(*,*)

   ! gamma(3) is related to the factorial function
   do i=1,20
      ! check value is not too big for default integer type
      if(factorial(i).gt.huge(0))then
         write(*,*)i,factorial(i)
      else
         write(*,*)i,factorial(i),int(factorial(i))
      endif
   enddo
   ! more factorials
   FAC: block
   integer,parameter :: n(*)=[0,1,5,11,170]
   integer :: j
      do j=1,size(n)
         write(*,'(*(g0,1x))')'factorial of', n(j),' is ', &
          & product([(real(i,kind=wp),i=1,n(j))]),  &
          & gamma(real(n(j)+1,kind=wp))
      enddo
   endblock FAC

   contains
   function factorial(i) result(f)
   integer,parameter :: dp=kind(0d0)
   integer,intent(in) :: i
   real :: f
      if(i.le.0)then
         write(*,'(*(g0))')'<ERROR> gamma(3) function value ',i,' <= 0'
         stop      '<STOP> bad value in gamma function'
      endif
      f=gamma(real(i+1))
   end function factorial
end program demo_gamma
```

Results:

```text
    gamma(1.0)=   1.000000
      1.000000       1.000000       2.000000       6.000000

              1   1.000000               1
              2   2.000000               2
              3   6.000000               6
              4   24.00000              24
              5   120.0000             120
              6   720.0000             720
              7   5040.000            5040
              8   40320.00           40320
              9   362880.0          362880
             10   3628800.         3628800
             11  3.9916800E+07    39916800
             12  4.7900160E+08   479001600
             13  6.2270208E+09
             14  8.7178289E+10
             15  1.3076744E+12
             16  2.0922791E+13
             17  3.5568741E+14
             18  6.4023735E+15
             19  1.2164510E+17
             20  2.4329020E+18
   factorial of 0  is  1.000000000000000 1.000000000000000
   factorial of 1  is  1.000000000000000 1.000000000000000
   factorial of 5  is  120.0000000000000 120.0000000000000
   factorial of 11  is  39916800.00000000 39916800.00000000
   factorial of 170  is  .7257415615307994E+307 .7257415615307999E+307
```

### **Standard**

Fortran 2008

### **See Also**

Logarithm of the Gamma function: [**log_gamma**(3)](#log_gamma)

### **Resources**

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)

 _fortran-lang intrinsic descriptions_

## get_command_argument

### **Name**

**get_command_argument**(3) - \[SYSTEM:COMMAND LINE\] Get command line arguments

### **Synopsis**
```fortran
  call get_command_argument(number [,value] [,length] &
  & [,status] [,errmsg])
```
```fortran
   subroutine get_command_argument( number, value, length, &
   & status ,errmsg)

    integer(kind=**),intent(in)             :: number
    character(len=*),intent(out),optional   :: value
    integer(kind=**),intent(out),optional   :: length
    integer(kind=**),intent(out),optional   :: status
    character(len=*),intent(inout),optional :: errmsg
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
   meeting the conditions described herein.
 - **number**, **length**, and **status** are scalar _integer_
   with a decimal exponent range of at least four.
 - **value** and **errmsg** are scalar _character_ variables of default
   kind.

### **Description**

**get_command_argument**(3) retrieves or queries the n-th argument that
was passed on the command line to the current program execution.

There is not anything specifically stated about what an argument is but
in practice the arguments are strings split on whitespace unless the
arguments are quoted. IFS values (Internal Field Separators) used by
common shells are typically ignored and unquoted whitespace is almost
always the separator.

Shells have often expanded command arguments and spell characters before
passing them to the program, so the strings read are often not exactly
what the user typed on the command line.

### **Options**

- **number**
  : is a non-negative number indicating which argument of the current
  program command line is to be retrieved or queried.
  : If **number = 0**, the argument pointed to is set to the name of the
  program (on systems that support this feature).
  : if the processor does not have such a concept as a command name the
  value of command argument 0 is processor dependent.
  : For values from 1 to the number of arguments passed to the program a
  value is returned in an order determined by the processor. Conventionally
  they are returned consecutively as they appear on the command line from
  left to right.

### **Result**

- **value**
  : The **value** argument holds the command line argument.
  If **value** can not hold the argument, it is truncated to fit the
  length of **value**.
  : If there are less than **number** arguments specified at the command
  line or if the argument specified does not exist for other reasons,
  **value** will be filled with blanks.

- **length**
  : The **length** argument contains the length of the n-th command
  line argument. The length of **value** has no effect on this value,
  It is the length required to hold all the significant characters of
  the argument regardless of how much storage is provided by **value**.

- **status**
  : If the argument retrieval fails, **status** is a positive number;
  if **value** contains a truncated command line argument, **status**
  is **-1**; and otherwise the **status** is zero.

### **Examples**

Sample program:
```fortran
program demo_get_command_argument
implicit none
character(len=255)           :: progname
integer                      :: count, i, argument_length, istat
character(len=:),allocatable :: arg

 ! command name assuming it is less than 255 characters in length
  call get_command_argument (0, progname, status=istat)
  if (istat == 0) then
     print *, "The program's name is " // trim (progname)
  else
     print *, "Could not get the program's name " // trim (progname)
  endif

 ! get number of arguments
  count = command_argument_count()
  write(*,*)'The number of arguments is ',count

  !
  ! allocate string array big enough to hold command line
  ! argument strings and related information
  !
  do i=1,count
     call get_command_argument(number=i,length=argument_length)
     if(allocated(arg))deallocate(arg)
     allocate(character(len=argument_length) :: arg)
     call get_command_argument(i, arg,status=istat)
     ! show the results
     write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
     & i,istat,argument_length,arg
  enddo

end program demo_get_command_argument
```
Results:
```bash
 ./demo_get_command_argument a  test 'of getting  arguments ' " leading"
```
```text
 The program's name is ./demo_get_command_argument
 The number of arguments is            4
001 00000 00001 [a]
002 00000 00004 [test]
003 00000 00022 [of getting  arguments ]
004 00000 00008 [ leading]
```
### **Standard**

Fortran 2003

### **See Also**

[**get_command**(3)](#get_command),
[**command_argument_count**(3)](#command_argument_count)

_fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## get_command

### **Name**

**get_command**(3) - \[SYSTEM:COMMAND LINE\] Get the entire command line invocation

### **Synopsis**
```fortran
    call get_command([command] [,length] [,status] [,errmsg])
```
```fortran
     subroutine get_command( command ,length ,status, errmsg )

      character(len=*),intent(out),optional   :: command
      integer(kind=**),intent(out),optional   :: length
      integer(kind=**),intent(out),optional   :: status
      character(len=*),intent(inout),optional :: errmsg
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
   meeting the conditions described herein.
 - **command** and **errmsg** are scalar _character_ variables of default kind.
 - **length** and **status** are scalar _integer_ with a decimal exponent
   range of at least four.

### **Description**

**get_command**(3) retrieves the entire command line that was used to
invoke the program.

Note that what is typed on the command line is often processed by
a shell. The shell typically processes special characters and white
space before passing it to the program. The processing can typically be
turned off by turning off globbing or quoting the command line arguments
and/or changing the default field separators, but this should rarely
be necessary.

### **Result**

- **command**
  : If **command** is present, the entire command line that was used
  to invoke the program is stored into it. If the command cannot be
  determined, **command** is assigned all blanks.

- **length**
  : If **length** is present, it is assigned the length of the command line.
  It is system-dependent as to whether trailing blanks will be counted.
  : If the command length cannot be determined, a length of 0 is assigned.

- **status**
  : If **status** is present, it is assigned 0 upon success of the
  command, **-1** if **command** is too short to store the command line,
  or a positive value in case of an error.

- **errmsg**
  : It is assigned a processor-dependent explanatory message if the
  command retrieval fails. Otherwise, it is unchanged.

### **Examples**

Sample program:

```fortran
program demo_get_command
implicit none
integer                      :: command_line_length
character(len=:),allocatable :: command_line
   ! get command line length
   call get_command(length=command_line_length)
   ! allocate string big enough to hold command line
   allocate(character(len=command_line_length) :: command_line)
   ! get command line as a string
   call get_command(command=command_line)
   ! trim leading spaces just in case
   command_line=adjustl(command_line)
   write(*,'("OUTPUT:",a)')command_line
end program demo_get_command
```
Results:
```bash
     # note that shell expansion removes some of the whitespace
     # without quotes
     ./test_get_command  arguments    on command   line to   echo

     OUTPUT:./test_get_command arguments on command line to echo

     # using the bash shell with single quotes
     ./test_get_command  'arguments  *><`~[]!{}?"\'| '

     OUTPUT:./test_get_command arguments  *><`~[]!{}?"'|
```
### **Standard**

Fortran 2003

### **See Also**

[**get_command_argument**(3)](#get_command_argument),
[**command_argument_count**(3)](#command_argument_count)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## get_environment_variable

### **Name**

**get_environment_variable**(3) - \[SYSTEM:ENVIRONMENT\] Get value of an environment variable

### **Synopsis**
```fortran
    call get_environment_variable(name [,value] [,length] &
    & [,status] [,trim_name] [,errmsg] )
```
```fortran
     subroutine character(len=*) get_environment_variable( &
     & name, value, length, status, trim_name, errmsg )

      character(len=*),intent(in) :: name
      character(len=*),intent(out),optional   :: value
      integer(kind=**),intent(out),optional   :: length
      integer(kind=**),intent(out),optional   :: status
      logical,intent(out),optional            :: trim_name
      character(len=*),intent(inout),optional :: errmsg
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
   meeting the conditions described herein.
 - **name**, **value**, and **errmsg**  are a scalar _character_ of
   default kind.
 - **length** and **status** are _integer_ scalars with a decimal exponent
   range of at least four.
 - **trim_name** is a scalar of type _logical_ and of default kind.

### **Description**

**get_environment_variable**(3) gets the **value** of the environment
variable **name**.

Note that **get_environment_variable**(3) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

If running in parallel be aware
It is processor dependent whether an environment variable that exists
on an image also exists on another image, and if it does exist on both
images whether the values are the same or different.

### **Options**

- **name**
  : The name of the environment variable to query.
  The interpretation of case is processor dependent.

### **Result**

- **value**
  : The value of the environment variable being queried. If **value**
  is not large enough to hold the data, it is truncated. If the variable
  **name** is not set or has no value, or the processor does not support
  environment variables **value** will be filled with blanks.

- **length**
  : Argument **length** contains the length needed for storing the
  environment variable **name**. It is zero if the environment variable
  is not set.

- **status**
  : **status** is **-1** if **value** is present but too short for the
  environment variable; it is **1** if the environment variable does
  not exist and **2** if the processor does not support environment
  variables; in all other cases **status** is zero.

- **trim_name**
  : If **trim_name** is present with the value _.false._, the trailing
  blanks in **name** are significant; otherwise they are not part of
  the environment variable name.

### **Examples**

Sample program:
```fortran
program demo_getenv
implicit none
character(len=:),allocatable :: homedir
character(len=:),allocatable :: var

     var='HOME'
     homedir=get_env(var)
     write (*,'(a,"=""",a,"""")')var,homedir

contains

function get_env(name,default) result(value)
! a function that makes calling get_environment_variable(3) simple
implicit none
character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
character(len=:),allocatable         :: value
integer                              :: howbig
integer                              :: stat
integer                              :: length
   length=0
   value=''
   if(name.ne.'')then
      call get_environment_variable( name, &
      & length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
       print *, name, " is not defined in the environment. Strange..."
       value=''
      case (2)
       print *, &
       "This processor does not support environment variables. Boooh!"
       value=''
      case default
       ! make string of sufficient size to hold value
       if(allocated(value))deallocate(value)
       allocate(character(len=max(howbig,1)) :: value)
       ! get value
       call get_environment_variable( &
       & name,value,status=stat,trim_name=.true.)
       if(stat.ne.0)value=''
      end select
   endif
   if(value.eq.''.and.present(default))value=default
end function get_env

end program demo_getenv
```
Typical Results:
```text
   HOME="/home/urbanjs"
```
### **Standard**

Fortran 2003

### **See also**

[**get_command_argument**(3)](#get_command_argument),
[**get_command**(3)](#get_command)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## huge

### **Name**

**huge**(3) - \[NUMERIC MODEL\] Largest number of a type and kind

### **Synopsis**
```fortran
    result = huge(x)
```
```fortran
     TYPE(kind=KIND) function huge(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

where **TYPE** may be _real_ or _integer_ and **KIND** is any supported
associated _kind_. It need not be defined, as only its characteristics
are used.

The result will be of the same type and kind as the input to ensure the
returned value does not overflow. Any assignment of the result should
take this into consideration.

### **Description**

**huge**(3) returns the largest number that is not an infinity for the
kind and type of **x**.

### **Options**

- **x**
  : **xx** is an arbitrary value which is used merely to determine what
  _kind_ and _type_ of scalar is being queried.

### **Result**

The return value is of the same type and kind as _x_ and is the
largest value supported by the specified model.

### **Examples**

Sample program:

```fortran
program demo_huge
implicit none
character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
integer :: i,j,k,biggest
real :: v, w
   ! basic
   print *, huge(0), huge(0.0), huge(0.0d0)
   print *, tiny(0.0), tiny(0.0d0)

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
end program demo_huge
```
Results:
```
  2147483647  3.4028235E+38  1.797693134862316E+308
  1.1754944E-38  2.225073858507201E-308

    1      6           6             6. T
    2      36          36            36. T
    3      216         216           216. T
    4      1296        1296          1296. T
    5      7776        7776          7776. T
    6      46656       46656         46656. T
    7      279936      279936        279936. T
    8      1679616     1679616       1679616. T
    9      10077696    10077696      10077696. T
    10     60466176    60466176      60466176. T
    11     362797056   362797056     362797056. T
    12    -2118184960 -2147483648    2176782336. F wrong for j and k and w
    13     175792128  -2147483648   13060694016. F wrong for j and k and w
    14     1054752768 -2147483648   78364164096. F wrong for j and k and w
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

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## hypot

### **Name**

**hypot**(3) - \[MATHEMATICS\] Returns the distance between the point
and the origin.

### **Synopsis**
```fortran
    result = hypot(x, y)
```
```fortran
     elemental real(kind=KIND) function hypot(x,y)

      real(kind=KIND),intent(in) :: x
      real(kind=KIND),intent(in) :: y
```
### **Characteristics**

 - **x,y** and the result shall all be of the same **kind**.

### **Description**

**hypot**(3) is referred to as the Euclidean distance function. It is
equal to
```fortran
sqrt(x**2+y**2)
```
without undue underflow or overflow.

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.

**hypot(x,y)** returns the distance between the point **<x,y>** and
the origin.

### **Options**

- **x**
: The type shall be _real_.

- **y**
  : The type and kind type parameter shall be the same as **x**.

### **Result**

The return value has the same type and kind type parameter as **x**.

The result is the positive magnitude of the distance of the point
**<x,y>** from the origin **<0.0,0.0>** .

### **Examples**

Sample program:

```fortran
program demo_hypot
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x, y
real(kind=real32),allocatable :: xs(:), ys(:)
integer :: i
character(len=*),parameter :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

   x = 1.e0_real32
   y = 0.5e0_real32

   write(*,*)
   write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
   write(*,'(*(g0))')'units away from the origin'
   write(*,*)

   ! elemental
   xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
   ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

   write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
   write(*,f)"have distances from the origin of ",hypot(xs,ys)
   write(*,f)"the closest is",minval(hypot(xs,ys))

end program demo_hypot
```

Results:

```text
   point <1.00000000,0.500000000> is 1.11803401
   units away from the origin

   the points
      +1.00000000 +0.500000000
      +1.00000000 +0.250000000
      +10.0000000 -10.0000000
      +15.0000000 +0.250000000
      -1.00000000 -0.250000000
   have distances from the origin of
      +1.11803401 +1.03077638
      +14.1421356 +15.0020828
      +1.03077638
   the closest is
      +1.03077638
```
### **Standard**

Fortran 2008

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## iachar

### **Name**

**iachar**(3) - \[CHARACTER:CONVERSION\] Return integer ASCII code of a character

### **Synopsis**
```fortran
    result = iachar(c [,kind])
```
```fortran
     elemental integer(kind=KIND) function iachar(c,kind)

      character(len=1),intent(in) :: c
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

 - The return value is of type _integer_ and of kind **KIND**. If **KIND**
   is absent, the return value is of default integer kind.
 - **kind** may be of any _integer_ kind.
 - a kind designated as ** may be any supported kind value for the type

### **Description**

  **iachar**(3) returns the code for the ASCII character in the first
  character position of C.

### **Options**

- **c**
  : A character to determine the ASCII code of.
  : A common extension is to allow strings but all but the first character
  is then ignored.

- **kind**
  : A constant initialization expression indicating the kind
  parameter of the result.

### **Result**

  the result is the position of the character **c** in the ASCII
  collating sequence. It is nonnegative and less than or equal to 127.

  By ASCII, it is meant that **c** is in the collating sequence defined
  by the codes specified in ISO/IEC 646:1991 (International Reference
  Version).

  The value of the result is processor dependent if **c** is not in the
  ASCII collating sequence.

  The results are consistent with the **lge**(3), **lgt**(3), **lle**(3),
  and **llt**(3) comparison functions. For example, if **lle(C, D)**
  is true, **iachar(C) <= iachar (D)** is true where **C** and **D**
  are any two characters representable by the processor.

### **Examples**

Sample program:

```fortran
program demo_iachar
implicit none
   ! basic usage
    ! just does a string one character long
    write(*,*)iachar('A')
    ! elemental: can do an array of letters
    write(*,*)iachar(['A','Z','a','z'])

   ! convert all characters to lowercase
    write(*,'(a)')lower('abcdefg ABCDEFG')
contains
!
pure elemental function lower(str) result (string)
! Changes a string to lowercase
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   ! step thru each letter in the string in specified range
   do i = 1, len(str)
      select case (str(i:i))
      case ('A':'Z') ! change letter to miniscule
         string(i:i) = char(iachar(str(i:i))+32)
      case default
      end select
   end do
end function lower
!
end program demo_iachar
```
Results:
```text
   65
   65          90          97         122
   abcdefg abcdefg
```
### **Standard**

  Fortran 95 , with KIND argument - Fortran 2003

### **See Also**

[**achar**(3)](#achar),
[**char**(3)](#char),
[**ichar**(3)](#ichar)

  See [**ichar**(3)](#ichar) in particular for a discussion of converting
  between numerical values and formatted string representations.

  Functions that perform operations on character strings, return lengths
  of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl), [**adjustr**(3)](#adjustr), [**index**(3)](#index),
  [**scan**(3)](#scan), [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## iall

### **Name**

**iall**(3) - \[BIT:LOGICAL\] Bitwise and of array elements

### **Synopsis**
```fortran
    result = iall(array [,mask])
```
```fortran
     integer(kind=KIND) function iall(array,mask)

      integer(kind=KIND),intent(in)        :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
  or
```fortran
    result = iall(array [,dim] [,mask])
```
```fortran
     integer(kind=KIND) function iall(array,dim,mask)

      integer(kind=KIND),intent(in)        :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
 - **array** must be an _integer_ array
 - **mask** is a _logical_ array that conforms to **array** of any
   _logical_ kind.
 - **dim** may be of any _integer_ kind.
 - The result will by of the same type and kind as **array**.

### **Description**

**iall**(3) reduces with a bitwise _and_ the elements of **array** along
dimension **dim** if the corresponding element in **mask** is _.true._.

### **Options**

- **array**
  : Shall be an array of type _integer_

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from **1 to n**, where **n** equals the rank of **array**.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Result**

The result is of the same type as **array**.

If **dim** is absent, a scalar with the bitwise _all_ of all elements in **array**
is returned. Otherwise, an array of rank **n-1**, where **n** equals the
rank of **array**, and a shape similar to that of **array** with dimension **dim**
dropped is returned.

### **Examples**

Sample program:

```fortran
program demo_iall
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
integer(kind=int8) :: a(2)

   a(1) = int(b'00100100')
   a(2) = int(b'01101010')

   print '(b8.8)', iall(a)

end program demo_iall
```

Results:

```text
   00100000
```

### **Standard**

Fortran 2008

### **See Also**

[**iany**(3)](#iany),
[**iparity**(3)](#iparity),
[**iand**(3)](#iand)

 _fortran-lang intrinsic descriptions_

## iand

### **Name**

**iand**(3) - \[BIT:LOGICAL\] Bitwise logical AND

### **Synopsis**
```fortran
    result = iand(i, j)
```
```fortran
     elemental integer(kind=KIND) function iand(i,j)

      integer(kind=KIND),intent(in) :: i
      integer(kind=KIND),intent(in) :: j
```
### **Characteristics**

- **i**, **j** and the result shall have the same _integer_ type and kind,
  with the exception that one of **i** or **j** may be a BOZ constant.

### **Description**

**iand**(3) returns the bitwise logical **and** of two values.

### **Options**

- **i**
  : one of the pair of values to compare the bits of

- **j**
  : one of the pair of values to compare the bits of

If either **i** or **j** is a BOZ-literal-constant, it is first converted
as if by the intrinsic function **int**(3)  to type _integer_ with the
kind type parameter of the other.

### **Result**

The result has the value obtained by combining **i** and **i**
bit-by-bit according to the following table:
```text
    I  |  J  |  IAND (I, J)
  ----------------------------
    1  |  1  |    1
    1  |  0  |    0
    0  |  1  |    0
    0  |  0  |    0
```
So if both the bit in **i** and **j** are on the resulting bit is on
(a one); else the resulting bit is off (a zero).

This is commonly called the "bitwise logical AND" of the two values.
### **Examples**

Sample program:

```fortran
program demo_iand
implicit none
integer :: a, b
 data a / z'f' /, b / z'3' /
 write (*,*) 'a=',a,' b=',b,'iand(a,b)=',iand(a, b)
 write (*,'(b32.32)') a,b,iand(a,b)
end program demo_iand
```
Results:
```text
    a= 15  b= 3 iand(a,b)= 3
   00000000000000000000000000001111
   00000000000000000000000000000011
   00000000000000000000000000000011
```
### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## iany

### **Name**

**iany**(3) - \[BIT:LOGICAL\] Bitwise OR of array elements

### **Synopsis**
```fortran
    result = iany(array [,mask])
```
```fortran
     integer(kind=KIND) function iany(array,mask)

      integer(kind=KIND),intent(in)        :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
or
```fortran
    result = iany(array [,dim] [,mask])
```
```fortran
     integer(kind=KIND) function iany(array,dim,mask)

      integer(kind=KIND),intent(in)        :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
 - **array** must be an array.
 - **dim** may be of any _integer_ kind.
 - **mask** is a _logical_ array that conforms to **array** of
   any _logical_ kind.
 - The result will by of the same type and kind
   as **array**.

### **Description**

  **iany**(3) reduces with bitwise a **OR** (inclusive **OR**) the
  elements of **array** along dimension **dim** if the corresponding
  element in **mask** is _.true._.

### **Options**

- **array**
  : an array of elements to selectively **OR** based on the mask.

- **dim**
  : a value in the range from **1 to n**, where **n** equals the rank
  of **array**.

- **mask**
  : a _logical_ scalar; or an array of the same shape as **array**.

### **Result**

  The result is of the same type as **array**.

  If **dim** is absent, a scalar with the bitwise _or_ of all elements in
  **array** is returned. Otherwise, an array of rank **n-1**, where **n**
  equals the rank of **array**, and a shape similar to that of **array**
  with dimension **dim** dropped is returned.

### **Examples**

Sample program:

```fortran
program demo_iany
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
logical,parameter :: T=.true., F=.false.
integer(kind=int8) :: a(3), b(4:3)
   a(1) = int(b'00100100')
   a(2) = int(b'01101010')
   a(3) = int(b'10101010')
   write(*,*)'A='
   print '(1x,b8.8)', a
   write(*,*)'IANY(A)='
   print '(1x,b8.8)', iany(a)

   write(*,*)'IANY(A) with a mask'
   print '(1x,b8.8)', iany(a,mask=[T,F,T])
   write(*,*)'should match '
   print '(1x,b8.8)', iany([a(1),a(3)])
   write(*,*)'does it?'
   write(*,*)iany(a,[T,F,T]) == iany([a(1),a(3)])
end program demo_iany
```
Results:
```text
    A=
    00100100
    01101010
    10101010
    IANY(A)=
    11101110
    IANY(A) with a mask
    10101110
    should match
    10101110
    does it?
    T
```
### **Standard**

Fortran 2008

### **See Also**

[**iparity**(3)](#iparity),
[**iall**(3)](#iall),
[**ior**(3)](#ior)

 _fortran-lang intrinsic descriptions_

### **Name**

**ibclr**(3) - \[BIT:SET\] Clear bit

### **Synopsis**
```fortran
    result = ibclr(i, pos)
```
```fortran
     elemental integer(kind=KIND) function ibclr(i,pos)

      integer(kind=KIND),intent(in) :: i
      integer(kind=**),intent(in) :: pos
```
### **Characteristics**
  - a kind designated as ** may be any supported kind value for the type

  - The return value is of the same kind as **i**. Otherwise,
    any _integer_ kinds are allowed.

### **Description**

  **ibclr**(3) returns the value of **i** with the bit at position **pos**
  set to zero.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit. **pos** is an **intent(in)** scalar or array of type
  _integer_. The value of **pos** must be within the range zero to
  **(bit_size(i)-1**).

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_

## ibits

### **Name**

**ibits**(3) - \[BIT:COPY\] Bit extraction

### **Synopsis**
```fortran
    result = ibits(i, pos, len)
```
```fortran
     elemental integer(kind=KIND) function ibits(i,pos,len)

      integer(kind=KIND),intent(in) :: i
      integer(kind=**),intent(in) :: pos
      integer(kind=**),intent(in) :: len
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type
  - The return value is of the same kind as **i**. Otherwise,
    any _integer_ kinds are allowed.
```fortran
```
### **Description**

**ibits**(3) extracts a field of length **len** from **i**, starting from
bit position **pos** and extending left for **len** bits. The result is
right-justified and the remaining bits are zeroed. The value of pos+len
must be less than or equal to the value **bit_size(i)**.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit.

- **len**
  : The type shall be _integer_.

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_

## ibset

### **Name**

**ibset**(3) - \[BIT:SET\] Set bit

### **Synopsis**
```fortran
    result = ibset(i, pos)
```
```fortran
     elemental integer(kind=KIND) function ibset(i,pos)

      integer(kind=KIND),intent(in) :: i
      integer(kind=**),intent(in) :: pos
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

  - The return value is of the same kind as **i**. Otherwise,
    any _integer_ kinds are allowed.

### **Description**

**ibset**(3) returns the value of **i** with the bit at position **pos** set to one.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit. pos is an **intent(in)** scalar or array of type
  _integer_. The value of pos must be within the range zero to
  **(bit_size(i)-1**).

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_

## ichar

### **Name**

**ichar**(3) - \[CHARACTER:CONVERSION\] Character-to-integer conversion function

### **Synopsis**
```fortran
    result = ichar(c [,kind])
```
```fortran
     elemental function ichar(c,kind)

      character(len=1),intent(in) :: c
      integer,intent(in),optional :: kind
```
### **Characteristics**

- **c** is a single scalar _character_
- **kind** a constant _integer_ initialization expression indicating
  the kind parameter of the result.
- The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default _integer_ kind.

### **Description**

 **ichar**(3) returns the code for the character in the system's native
 character set. The correspondence between characters and their codes is
 not necessarily the same across different Fortran implementations. For
 example, a platform using EBCDIC would return different values than an
 ASCII platform.

 See **iachar**(3) for specifically working with the ASCII character set.

### **Options**

- **c**
  : The input character to determine the code for.

- **kind**
  : indicates the kind parameter of the result.  If **kind** is absent,
  the return value is of default _integer_ kind.

### **Result**

The code in the systems default character set for the character being queried.

### **Examples**

Sample program:

```fortran
program demo_ichar
implicit none
integer i

   write(*,*)ichar(['a','z','A','Z'])

end program demo_ichar
```
Results:
```text
             97         122          65          90
```
### **Standard**

Fortran 95 , with KIND argument -Fortran 2003

### **See Also**

[**achar**(3)](#achar),
[**char**(3)](#char),
[**iachar**(3)](#iachar)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),

[**scan**(3)](#scan),
[**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions_

## ieor

### **Name**

**ieor**(3) - \[BIT:LOGICAL\] Bitwise logical exclusive or

### **Synopsis**
```fortran
    result = ieor(i, j)
```
```fortran
     elemental integer(kind=KIND) function ieor(i,j)

      integer(kind=**),intent(in) :: i
      integer(kind=**),intent(in) :: j
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

  - any _integer_ kinds are allowed for **i** and **j**.

  - The return value is of the same kind as the larger kind of **i**
    and **j**.

### **Description**

**ieor**(3) returns the bitwise Boolean exclusive-**or** of **i** and **j**.

### **Options**

- **i**
  : The type shall be _integer_.

- **j**
  : The type shall be _integer_, of the same kind as **i**.

### **Result**

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_

## image_index

### **Name**

**image_index**(3) - \[COLLECTIVE\] Cosubscript to image index conversion

### **Synopsis**
```fortran
    result = image_index(coarray, sub)
```
```fortran
```
### **Characteristics**

### **Description**

**image_index**(3) returns the image index belonging to a cosubscript.

### **Options**

- **coarray**
  : Coarray of any type.

- **sub**
  : default integer rank-1 array of a size equal to the corank of
  **coarray**.

### **Result**

Scalar default integer with the value of the image index which
corresponds to the cosubscripts. For invalid cosubscripts the result is
zero.

### **Examples**

Sample program:

```fortran
program demo image_index
implicit none
integer :: array[2,-1:4,8,*]
   ! Writes  28 (or 0 if there are fewer than 28 images)
   write (*,*) image_index(array, [2,0,3,1])
end demo image_index
```

### **Standard**

Fortran 2008

### **See Also**

[**this_image**(3)](#this_image),
[**num_images**(3)](#num_images)

 _fortran-lang intrinsic descriptions_

## index

### **Name**

**index**(3) - \[CHARACTER:SEARCH\] Position of a substring within a string

### **Synopsis**
```fortran
result = index( string, substring [,back] [,kind] )
```
```fortran
 elemental integer(kind=KIND) function index(string,substring,back,kind)

  character(len=*),intent(in) :: string
  character(len=*),intent(in) :: substring
  logical,intent(in),optional :: back
  integer,intent(in),optional :: kind
```
### **Characteristics**

### **Description**

  **index**(3) returns the position of the start of the leftmost
  or rightmost occurrence of string **substring** in **string**,
  counting from one. If **substring** is not present in **string**,
  zero is returned.

### **Options**

- **string**
  : string to be searched

- **substring**
  : string to attempt to locate in **string**

- **back**
  : If the **back** argument is present and true, the return value is the
  start of the rightmost occurrence rather than the leftmost.

- **kind**
  : An _integer_ initialization expression indicating the kind parameter
  of the result.

### **Result**

- **START**
  : The return value is of type _integer_ and of kind **kind**. If **kind** is
  absent, the return value is of default integer kind.

### **Examples**

Example program

```fortran
program demo_index
implicit none
character(len=*),parameter :: str=&
   'Search this string for this expression'
   !1234567890123456789012345678901234567890
   write(*,*)&
      index(str,'this').eq.8,              &
      ! return value is counted from the left end even if BACK=.TRUE.
      index(str,'this',back=.true.).eq.24, &
      ! INDEX is case-sensitive
      index(str,'This').eq.0
end program demo_index
```

Expected Results:

```text
   T T T
```

### **Standard**

FORTRAN 77 , with KIND argument Fortran 2003

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl), [**adjustr**(3)](#adjustr), [**index**(3)](#index),
  [**scan**(3)](#scan), [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions_

## int

### **Name**

**int**(3) - \[TYPE:NUMERIC\] Convert to integer type by truncating towards zero

### **Synopsis**
```fortran
    result = int(a [,kind])
```
```fortran
     elemental integer(kind=KIND) function int(a, kind )

      TYPE(kind=KIND),intent(in) :: a
      integer,optional :: kind
```
### **Characteristics**

### **Description**

**int**(3) converts a value to _integer_ type by truncating towards zero.

### **Options**

- **a**
  : Shall be of type _integer_, _real_, or _complex_ or a BOZ-literal-constant.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

  If not present the returned type is that of default integer type.

### **Result**

returns an _integer_ variable or array applying the following rules:

**Case**:

1.  If **a** is of type _integer_, **int**(a) = a

2.  If **a** is of type _real_ and **|a| \< 1, int(a)** equals **0**. If **|a| \>=
    1**, then **int(a)** equals the integer whose magnitude does not exceed
    **a** and whose sign is the same as the sign of **a**.

3.  If **a** is of type _complex_, rule 2 is applied to the _real_ part of **a**.

4.  If _a_ is a boz-literal constant, it is treated as an _integer_
    with the _kind_ specified.

    The interpretation of a bit sequence whose most significant bit is
    **1** is processor dependent.

The result is undefined if it cannot be represented in the specified integer type.

### **Examples**

Sample program:

```fortran
program demo_int
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer :: i = 42
complex :: z = (-3.7, 1.0)
real :: x=-10.5, y=10.5

   print *, int(x), int(y)

   print *, int(i)

   print *, int(z), int(z,8)
   ! elemental
   print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])
   ! note int(3) truncates towards zero

   ! CAUTION:
   ! a number bigger than a default integer can represent
   ! produces an incorrect result and is not required to
   ! be detected by the program.
   x=real(huge(0))+1000.0
   print *, int(x),x
   ! using a larger kind
   print *, int(x,kind=int64),x

   print *, int(&
   & B"111111111111111111111111111111111111111111111111111111111111111",&
   & kind=int64)
   print *, int(O"777777777777777777777",kind=int64)
   print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)

   ! elemental
   print *
   print *,int([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_int
```

Results:

```text
            -10   10
             42
             -3  -3
            -10  -10  -10   10   10  10
    -2147483648   2.14748467E+09
     2147484672   2.14748467E+09
     9223372036854775807
     9223372036854775807
     9223372036854775807

    -2          -2          -2          -2          -1
    -1           0           0           0           1
     1           2           2           2           2
```

### **Standard**

FORTRAN 77

### **See Also**

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**nint**(3)](#nint),
[**selected_int_kind**(3)](#selected_int_kind),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## ior

### **Name**

**ior**(3) - \[BIT:LOGICAL\] Bitwise logical inclusive OR

### **Synopsis**
```fortran
    result = ior(i, j)
```
```fortran
     elemental integer(kind=KIND) function ior(i,j)

      integer(kind=KIND ,intent(in) :: i
      integer(kind=KIND ,intent(in) :: j
```
### **Characteristics**

- **i**, **j** and the result shall have the same _integer_ type and kind,
  with the exception that one of **i** or **j** may be a BOZ constant.

### **Description**

**ior**(3) returns the bit-wise Boolean inclusive-or of **i** and **j**.

### **Options**

- **i**
  : one of the pair of values to compare the bits of

- **j**
  : one of the pair of values to compare the bits of

If either **i** or **j** is a BOZ-literal-constant, it is first converted
as if by the intrinsic function **int**(3)  to type _integer_ with the
kind type parameter of the other.

### **Result**

 The result has the value obtained by combining I and J
 bit-by-bit according to the following table:
```text
          I   J   IOR (I, J)
          1   1        1
          1   0        1
          0   1        1
          0   0        0
```
 This is commonly called the "bitwise logical inclusive OR" of the two values.

### **Examples**

Sample program:
```fortran
program demo_ior
implicit none
integer :: i, j, k
   i=53       ! i=00110101 binary (lowest order byte)
   j=45       ! j=00101101 binary (lowest order byte)
   k=ior(i,j) ! k=00111101 binary (lowest order byte), k=61 decimal
   write(*,'(i8,1x,b8.8)')i,i,j,j,k,k
end program demo_ior
```
Results:
```
         53 00110101
         45 00101101
         61 00111101
```
### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## iparity

### **Name**

**iparity**(3) - \[BIT:LOGICAL\] Bitwise exclusive or of array elements

### **Synopsis**
```fortran
    result = iparity(array [,mask])
```
```fortran
     integer(kind=KIND) function iparity(array, mask )

      integer(kind=KIND),intent(in) :: array(..)
      logical(kind=KIND),intent(in),optional :: mask(..)
```
   **array** must be an array. **mask** may be either an array of the
   same shape as **array** or a scalar.

or
```fortran
    result = iparity(array, dim [,mask] )
```
```fortran
     integer(kind=KIND) function iparity( array ,dim ,mask )

      integer(kind=KIND),intent(in)          :: array(..)
      logical(kind=KIND),intent(in)          :: dim
      logical(kind=KIND),intent(in),optional :: mask(..)
```
### **Description**

**iparity**(3) reduces with bitwise _xor_ (exclusive _or_) the elements
of **array** along dimension **dim** if the corresponding element in
**mask** is _.true._.

### **Options**

- **array**
  : Shall be an array of type _integer_

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from **"1" to "n"**, where **"n"** equals the rank of **array**.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Result**

The result is of the same type as **array**.

If **dim** is absent, a scalar with the bitwise _xor_ of all elements in **array**
is returned. Otherwise, an array of rank **n-1**, where **n** equals the
rank of **array**, and a shape similar to that of **array** with dimension **dim**
dropped is returned.

### **Examples**

Sample program:
```fortran
program demo_iparity
implicit none
integer, dimension(2) :: a
  a(1) = int(b'00100100')
  a(2) = int(b'01101010')
  print '(b8.8)', iparity(a)
end program demo_iparity
```
Results:
```
   01001110
```
### **Standard**

Fortran 2008

### **See Also**

[**iany**(3)](#iany),
[**iall**(3)](#iall),
[**ieor**(3)](#ieor),
[**parity**(3)](#parity)

 _fortran-lang intrinsic descriptions_

## is_contiguous

### **Name**

**is_contiguous**(3) - \[ARRAY INQUIRY\] Test if object is contiguous

### **Synopsis**
```fortran
    result = is_contiguous(a)
```
```fortran
     logical function is_contigious(a)

      type(TYPE(kind=**)),intent(in) :: a
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **a** may be of any type. It shall be an array. If it is a pointer it
  shall be associated.
- the result is a default logical scalar

### **Description**

**is_contigious**(3) returns _.true._ if and only if an object is
contiguous.

An object is contiguous if it is

- **(1)**
  an object with the CONTIGUOUS attribute,

- **(2)**
  a nonpointer whole array that is not assumed-shape,

- **(3)**
  an assumed-shape array that is argument associated with an array
  that is contiguous,

- **(4)**
  an array allocated by an ALLOCATE statement,

- **(5)**
  a pointer associated with a contiguous target, or

- **(6)**
  a nonzero-sized array section provided that

  - **(a)**
    its base object is contiguous,

  - **(b)**
    it does not have a vector subscript,

  - **(c)**
    the elements of the section, in array element order, are a
    subset of the base object elements that are consecutive in
    array element order,

  - **(d)**
    if the array is of type character and a substring-range
    appears, the substring-range specifies all of the characters
    of the parent-string,

  - **(e)**
    only its final part-ref has nonzero rank, and

  - **(f)**
    it is not the real or imaginary part of an array of type
    complex.

An object is not contiguous if it is an array subobject, and

- the object has two or more elements,

- the elements of the object in array element order are not
  consecutive in the elements of the base object,

- the object is not of type character with length zero, and

- the object is not of a derived type that has no ultimate
  components other than zero-sized arrays and

- characters with length zero.

It is processor-dependent whether any other object is contiguous.

### **Options**

- **a**
  : An array of any type to be tested for being contigious. If it is a
  pointer it shall be associated.

### **Result**

  The result has the value _.true._ if **a** is contiguous, and _.false._
  otherwise.

### **Examples**

Sample program:
```fortran
program demo_is_contiguous
implicit none
intrinsic is_contiguous
real, DIMENSION (1000, 1000), TARGET :: A
real, DIMENSION (:, :), POINTER       :: IN, OUT
   IN => A              ! Associate IN with target A
   OUT => A(1:1000:2,:) ! Associate OUT with subset of target A
   !
   write(*,*)'IN is ',IS_CONTIGUOUS(IN)
   write(*,*)'OUT is ',IS_CONTIGUOUS(OUT)
   !
end program demo_is_contiguous
```
Results:

```text
    IN is  T
    OUT is  F
```
### **Standard**

Fortran 2008

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## ishftc

### **Name**

**ishftc**(3) - \[BIT:SHIFT\] Shift rightmost bits circularly, aka. a logical shift

### **Synopsis**
```fortran
    result = ishftc( i, shift [,size] )
```
```fortran
     elemental integer(kind=KIND) function ishftc(i, shift, size)

      integer(kind=KIND),intent(in)     :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
      integer(kind=SIZEKIND),intent(in),optional  :: size
```
### **Characteristics**

  where KIND, SHIFTKIND, and SIZEKIND  may be any supported _integer_
  kind, but where the kind for **i** dictates the kind of the returned
  value.

### **Description**

  **ishftc**(3) circularly shifts just the specified rightmost bits of
  an integer.

  **ishftc**(3) returns a value corresponding to **i** with the rightmost
  **size** bits shifted circularly **shift** places; that is, bits
  shifted out one end of the section are shifted into the opposite end
  of the section.

  A value of **shift** greater than zero corresponds to a left shift,
  a value of zero corresponds to no shift, and a value less than zero
  corresponds to a right shift.

### **Options**

- **i**
  : The value specifying the pattern of bits to shift

- **shift**
  : If **shift** is positive, the shift is to the left; if **shift**
  is negative, the shift is to the right; and if **shift** is zero,
  no shift is performed. The absolute value of **shift** must be less
  than **size** (simply put, the number of bits to shift must be less
  than or equal to the number of bits specified to be shifting!)

- **size**
  : (Optional) The type shall be _integer_; the value must be greater than
  zero and less than or equal to **bit_size**(i).
  The default is **bit_size(i)**. That is, the default is to circularly shift
  the entire value **i**.

### **Result**

  The result characteristics (kind, shape, size, ranke, ...) are the same as **i**.

  The result has the value obtained by shifting the **size** rightmost
  bits of **i** circularly by **shift** positions.

  No bits are lost.

  The unshifted bits are unaltered.

### **Examples**

Sample program:
```fortran
program demo_ishftc
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer             :: shift
integer             :: i
character(len=*),parameter :: g='(b32.32,1x,i0)'

   write(*,*) ishftc(3, 1),' <== typically should have the value 6'

  ! shift a value by various amounts
   do i= -bit_size(0), bit_size(0), 8
      write(*,g) ishftc(huge(0),i), i
   enddo

end program demo_ishftc
```
Results:
```text
>              6  <== typically should have the value 6
>   01111111111111111111111111111111 -32
>   11111111111111111111111101111111 -24
>   11111111111111110111111111111111 -16
>   11111111011111111111111111111111 -8
>   01111111111111111111111111111111 0
>   11111111111111111111111101111111 8
>   11111111111111110111111111111111 16
>   11111111011111111111111111111111 24
>   01111111111111111111111111111111 32
```

### **Standard**

Fortran 95

### **See Also**

[**ishft**(3)](#ishft)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## ishft

### **Name**

**ishft**(3) - \[BIT:SHIFT\] Shift bits

### **Synopsis**
```fortran
    result = ishftc( i, shift )
```
```fortran
     elemental integer(kind=KIND) function ishft(i, shift )

      integer(kind=KIND),intent(in) :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
```
### **Characteristics**

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

Fortran 95

### **See Also**

[**ishftc**(3)](#ishftc)

 _fortran-lang intrinsic descriptions_

## is_iostat_end

### **Name**

**is_iostat_end**(3) - \[STATE\] Test for end-of-file value

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

## is_iostat_eor

### **Name**

**is_iostat_eor**(3) - \[STATE\] Test for end-of-record value

### **Synopsis**
```fortran
    result = is_iostat_eor(i)
```
```fortran
     elemental integer function is_iostat_eor(i)

      integer(kind=KIND),intent(in) :: iostat
```
### **Characteristics**

### **Description**

**is_iostat_eor**(3) tests whether a variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the **iostat_eor** parameter of the intrinsic module
**iso_fortran_env**.

### **Options**

- **i**
  : The value to test as indicating "end of record".

### **Result**

Returns a _logical_ of the default kind, which is _.true._ if **i**
has the value which indicates an end of file condition for iostat=
specifiers, and is _.false._ otherwise.

### **Examples**

Sample program:

```fortran
program demo_is_iostat_eor
use iso_fortran_env, only : iostat_eor
implicit none
integer :: inums(50), lun, ios

  open(newunit=lun, file='_test.dat', form='unformatted')
  write(lun, '(a)') '10 20 30'
  write(lun, '(a)') '40 50 60 70'
  write(lun, '(a)') '80 90'
  write(lun, '(a)') '100'

  do
     read(lun, *, iostat=ios) inums
     write(*,*)'iostat=',ios
     if(is_iostat_eor(ios)) stop 'end of record'
  enddo

  close(lun,iostat=ios,status='delete')

end program demo_is_iostat_eor
```
### **Standard**

Fortran 2003

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## kind

### **Name**

**kind**(3) - \[KIND INQUIRY\] Kind of an entity

### **Synopsis**
```fortran
    result = kind(x)
```
```fortran
     integer function kind(x)

      type(TYPE,kind=KIND),intent(in) :: x(..)
```
### **Characteristics**

  **TYPE** may _logical_, _integer_, _real_, _complex_ or _character_.

  **x** may be of any kind supported by the type, and may be
  scalar or an array.

### **Description**

   **kind(x)**(3) returns the kind value of the entity **x**.

### **Options**

- **x**
  : Value to query the kind of.

### **Result**

  The return value indicates the kind of the argument **x**.

  Note that kinds are processor-dependent.

### **Examples**

Sample program:
```fortran
program demo_kind
implicit none
integer,parameter :: dc = kind(' ')
integer,parameter :: dl = kind(.true.)

   print *, "The default character kind is ", dc
   print *, "The default logical kind is ", dl

end program demo_kind
```
Results:

```text
    The default character kind is            1
    The default logical kind is            4
```
### **Standard**

Fortran 95

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## lbound

### **Name**

**lbound**(3) - \[ARRAY INQUIRY\] Lower dimension bounds of an array

### **Synopsis**
```fortran
    result = lbound(array [,dim] [,kind] )
```
```fortran
     elemental TYPE(kind=KIND) function lbound(array,dim,kind)

      TYPE(kind=KIND),intent(in)  :: array
      integer,intent(in),optional :: dim
      integer,intent(in),optional :: kind
```
### **Characteristics**

- **array** shall be an array, of any type.
- **dim** shall be a scalar _integer_.
- **kind** an _integer_ initialization expression indicating the kind
  parameter of the result.
- The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default integer kind.

### **Description**

  **result**(3) returns the lower bounds of an array, or a single lower
  bound along the **dim** dimension.

### **Options**

- **array**
  : Shall be an array, of any type.

- **dim**
  : Shall be a scalar _integer_.
  If **dim** is absent, the result is an array of the upper bounds of
  **array**.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is absent, the
result is an array of the lower bounds of **array**. If **dim** is present, the
result is a scalar corresponding to the lower bound of the array along
that dimension. If **array** is an expression rather than a whole array or
array structure component, or if it has a zero extent along the relevant
dimension, the lower bound is taken to be 1.

### **Examples**

Note that in my opinion this function should not be used on assumed-size
arrays or in any function without an explicit interface. Errors can
occur if there is no interface defined.

Sample program

```fortran
! program demo_lbound
module m_bounds
implicit none
 contains
    subroutine msub(arr)
       !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
       integer,intent(in) :: arr(:)
       write(*,*)'MSUB: LOWER=',lbound(arr), &
       & 'UPPER=',ubound(arr), &
       & 'SIZE=',size(arr)
    end subroutine msub
 end module m_bounds

 use m_bounds, only : msub
 implicit none
 interface
    subroutine esub(arr)
    integer,intent(in) :: arr(:)
    end subroutine esub
 end interface
 integer :: arr(-10:10)
    write(*,*)'MAIN: LOWER=',lbound(arr), &
    & 'UPPER=',ubound(arr), &
    & 'SIZE=',size(arr)
    call csub()
    call msub(arr)
    call esub(arr)
 contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr), &
   & 'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub
end

 subroutine esub(arr)
 implicit none
 integer,intent(in) :: arr(:)
    ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
    ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
    write(*,*)'ESUB: LOWER=',lbound(arr), &
    & 'UPPER=',ubound(arr), &
    & 'SIZE=',size(arr)
 end subroutine esub

!end program demo_lbound
```

Results:

```
   MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
   CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
   MSUB: LOWER=           1 UPPER=          21 SIZE=          21
   ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```

### **Standard**

Fortran 95 , with KIND argument - Fortran 2003

### **See Also**

#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

[**ubound**(3)](#ubound),
[**co_lbound**(3)](#co_lbound)

#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contigious) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry:

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## leadz

### **Name**

**leadz**(3) - \[BIT:COUNT\] Number of leading zero bits of an integer

### **Synopsis**
```fortran
    result = leadz(i)
```
```fortran
     elemental integer function leadz(i)

      integer(kind=KIND),intent(in) :: i
```
### **Characteristics**

### **Description**

**leadz**(3) returns the number of leading zero bits of an integer.

### **Options**

- **i**
  : _integer_ to count the leading zero bits of.

### **Result**

The type of the return value is the same as a default _integer_. If all
the bits of **i** are zero, the result value is **bit_size(i)**.

### **Examples**

Sample program:

```fortran
program demo_leadz
implicit none
integer :: value, i
character(len=80) :: f

  ! make a format statement for writing a value as a bit string
  write(f,'("(b",i0,".",i0,")")')bit_size(value),bit_size(value)

  ! show output for various integer values
  value=0
  do i=-150, 150, 50
     value=i
     write (*,'("LEADING ZERO BITS=",i3)',advance='no') leadz(value)
     write (*,'(" OF VALUE ")',advance='no')
     write(*,f,advance='no') value
     write(*,'(*(1x,g0))') "AKA",value
  enddo
  ! Notes:
  ! for two's-complements programming environments a negative non-zero
  ! integer value will always start with a 1 and a positive value with 0
  ! as the first bit is the sign bit. Such platforms are very common.
end program demo_leadz
```
Results:
```text
  LEADING ZERO BITS=  0 OF VALUE 11111111111111111111111101101010 AKA -150
  LEADING ZERO BITS=  0 OF VALUE 11111111111111111111111110011100 AKA -100
  LEADING ZERO BITS=  0 OF VALUE 11111111111111111111111111001110 AKA -50
  LEADING ZERO BITS= 32 OF VALUE 00000000000000000000000000000000 AKA 0
  LEADING ZERO BITS= 26 OF VALUE 00000000000000000000000000110010 AKA 50
  LEADING ZERO BITS= 25 OF VALUE 00000000000000000000000001100100 AKA 100
  LEADING ZERO BITS= 24 OF VALUE 00000000000000000000000010010110 AKA 150
```
### **Standard**

Fortran 2008

### **See Also**

[**bit_size**(3)](#bit_size),
[**popcnt**(3)](#popcnt),
[**poppar**(3)](#poppar),
[**trailz**(3)](#trailz)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## len

### **Name**

**len**(3) - \[CHARACTER\] Length of a character entity

### **Synopsis**
```fortran
    result = len(string [,kind])
```
```fortran
     integer(kind=KIND) function len(string,kind)

      character(len=*),intent(in) :: string
      integer,optional,intent(in) :: kind
```
### **Characteristics**

  where the returned value is the same integer kind as the **kind**
  argument, or of the default integer kind if **kind** is not specified.

### **Description**

  **len**(3) returns the length of a _character_ string.

  If **string** is an array, the length of an element of **string** is
  returned, as all elements of an array are the same length.

  Note that **string** need not be defined when this intrinsic is invoked,
  as only the length (not the content) of **string** is needed.

### **Options**

- **string**
  : A scalar or array of type _character_ to return the length of

- **kind**
  : A constant _integer_ initialization expression indicating the _kind_
  parameter of the result.

### **Result**

  The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default integer kind.

### **Examples**

Sample program

```fortran
program demo_len
implicit none

! fixed length
character(len=40) :: string
! allocatable length
character(len=:),allocatable :: astring
character(len=:),allocatable :: many_strings(:)
integer :: ii
  ! BASIC USAGE
   ii=len(string)
   write(*,*)'length =',ii

  ! ALLOCATABLE VARIABLE LENGTH CAN CHANGE
  ! the allocatable string length will be the length of RHS expression
   astring=' How long is this allocatable string? '
   write(*,*)astring, ' LEN=', len(astring)
  ! print underline
   write(*,*) repeat('=',len(astring))
  ! assign new value to astring and length changes
   astring='New allocatable string'
   write(*,*)astring, ' LEN=', len(astring)
  ! print underline
   write(*,*) repeat('=',len(astring))

  ! THE STRING LENGTH WILL BE CONSTANT FOR A FIXED-LENGTH VARIABLE
   string=' How long is this fixed string? '
   write(*,*)string,' LEN=',len(string)
   string='New fixed string '
   write(*,*)string,' LEN=',len(string)

  ! ALL STRINGS IN AN ARRAY ARE THE SAME LENGTH
  ! a scalar is returned for an array, as all values in a Fortran
  ! character array must be of the same length.
   many_strings = [ character(len=7) :: 'Tom', 'Dick', 'Harry' ]
   write(*,*)'length of ALL elements of array=',len(many_strings)

  ! NAME%LEN IS ESSENTIALLY THE SAME AS LEN(NAME)
  ! you can also query the length (and other attributes) of a string
  ! using a "type parameter inquiry" (available since fortran 2018)
   write(*,*)'length from type parameter inquiry=',string%len
  ! %len is equivalent to a call to LEN() except the kind of the integer
  ! value returned is always of default kind.

  ! LOOK AT HOW A PASSED STRING CAN BE USED ...
   call passed(' how long? ')

contains

   subroutine passed(str)
   character(len=*),intent(in)  :: str
   ! the length of str can be used in the definitions of variables
      ! you can query the length of the passed variable
      write(*,*)'length of passed value is ', LEN(str)
   end subroutine passed

end program demo_len
```
Results:
```text
    length =          40
     How long is this allocatable string?  LEN=          38
    ======================================
    New allocatable string LEN=          22
    ======================
     How long is this fixed string?          LEN=          40
    New fixed string                         LEN=          40
    length of ALL elements of array=           7
    length from type parameter inquiry=          40
    length of passed value is           11
```
### **Standard**

FORTRAN 77 ; with **kind** argument - Fortran 2003

### **See Also**

len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines that
allow you to deal with leading and trailing blanks.

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),
  [**scan**(3)](#scan),
  [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim), [**len**(3)](#len), [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## len_trim

### **Name**

**len_trim**(3) - \[CHARACTER:WHITESPACE\] Character length without trailing blank characters

### **Synopsis**
```fortran
  result = len_trim(string [,kind])
```
```fortran
   elemental integer(kind=KIND) function len_trim(string,kind)

    character(len=*),intent(in) :: string
    integer(kind=KIND),intent(in),optional :: kind
```
### **Characteristics**

**string** is a scalar or array of type _character_

**kind** is a scalar integer constant expression specifying the kind
of the returned value.

The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default _integer_ kind.

### **Description**

**len_trim**(3) returns the length of a character string, ignoring any
trailing blanks.

### **Options**

- **string**
  : The input string whose length is to be measured.

- **kind**
  : Indicates the kind parameter of the result.

### **Result**

  The result has a value equal to the number of characters remaining
  after any trailing blanks in STRING are removed. If the argument
  contains no nonblank characters, the result is zero.

### **Examples**

Sample program
```fortran
program demo_len_trim
implicit none
character(len=:),allocatable :: string
integer :: i
! basic usage
   string=" how long is this string?     "
   write(*,*) string
   write(*,*)'UNTRIMMED LENGTH=',len(string)
   write(*,*)'TRIMMED LENGTH=',len_trim(string)

   ! print string, then print substring of string
   string='xxxxx   '
   write(*,*)string,string,string
   i=len_trim(string)
   write(*,*)string(:i),string(:i),string(:i)
   !
  ! elemental example
   ELE:block
   ! an array of strings may be used
   character(len=:),allocatable :: tablet(:)
   tablet=[character(len=256) :: &
   & ' how long is this string?     ',&
   & 'and this one?']
      write(*,*)'UNTRIMMED LENGTH=  ',len(tablet)
      write(*,*)'TRIMMED LENGTH=    ',len_trim(tablet)
      write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
   endblock ELE
   !
end program demo_len_trim
```
Results:
```text
     how long is this string?
    UNTRIMMED LENGTH=          30
    TRIMMED LENGTH=          25
    xxxxx   xxxxx   xxxxx
    xxxxxxxxxxxxxxx
    UNTRIMMED LENGTH=           256
    TRIMMED LENGTH=              25          13
    SUM TRIMMED LENGTH=          38
```
### **Standard**

Fortran 95 . **kind** argument added with Fortran 2003.

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),
  [**scan**(3)](#scan),
  [**verify**(3)](#verify)

- **Nonelemental:**
  [**repeat**(3)](#repeat),
  [**len**(3)](#len),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## lge

### **Name**

**lge**(3) - \[CHARACTER:COMPARE\] ASCII Lexical greater than or equal

### **Synopsis**
```fortran
    result = lge(string_a, stringb)
```
```fortran
     elemental logical function lge(string_a, string_b)

      character(len=*),intent(in) :: string_a
      character(len=*),intent(in) :: string_b
```
### **Characteristics**

### **Description**

  **lge**(3) determines whether one string is lexically greater than
  or equal to another string, where the two strings are interpreted as
  containing ASCII character codes. If the String **a** and String **b**
  are not the same length, the shorter is compared as if spaces were
  appended to it to form a value that has the same length as the longer.

  The lexical comparison intrinsics **lge**(3), **lgt**(3), **lle**(3),
  and **llt**(3) differ from the corresponding intrinsic operators
  _.ge., .gt., .le., and .lt._, in that the latter use the processor's
  character ordering (which is not ASCII on some targets), whereas the
  former always use the ASCII ordering.

### **Options**

- **string_a**
  : Shall be of default _character_ type.

- **string_b**
  : Shall be of default _character_ type.

### **Result**

Returns _.true._ if string_a == string_b, and _.false._ otherwise,
based on the ASCII ordering.

If both input arguments are null strings, _.true._ is always returned.

### **Examples**

Sample program:
```fortran
program demo_lge
implicit none
integer :: i
   write(*,'(*(a))')(char(i),i=32,126)  ! ASCII order
   write(*,*) lge('abc','ABC')          ! [T] lowercase is > uppercase
   write(*,*) lge('abc','abc  ')        ! [T] trailing spaces
   ! If both strings are of zero length the result is true
   write(*,*) lge('','')                ! [T]
   write(*,*) lge('','a')               ! [F] the null string is padded
   write(*,*) lge('a','')               ! [T]
   write(*,*) lge('abc',['abc','123'])  ! [T T]  scalar and array
   write(*,*) lge(['cba', '123'],'abc') ! [T F]
   write(*,*) lge(['abc','123'],['cba','123']) ! [F T]  both arrays
end program demo_lge
```
Results:
```text
    !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
    [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
    T
    T
    T
    F
    T
    T T
    T F
    F T
```
### **Standard**

FORTRAN 77

### **See Also**

  [**lgt**(3)](#lgt),
  [**lle**(3)](#lle),
  [**llt**(3)](#llt)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),

[**scan**(3)](#scan),
[**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## lgt

### **Name**

**lgt**(3) - \[CHARACTER:COMPARE\] ASCII Lexical greater than

### **Synopsis**
```fortran
     result = lgt(string_a, stringb)
```
```fortran
      elemental logical function lgt(string_a, string_b)

       character(len=*),intent(in) :: string_a
       character(len=*),intent(in) :: string_b
```
### **Characteristics**

### **Description**

  **lgt**(3) determines whether one string is lexically greater than
  another string, where the two strings are interpreted as containing
  ASCII character codes. If the String **a** and String **b** are not
  the same length, the shorter is compared as if spaces were appended
  to it to form a value that has the same length as the longer.

  In general, the lexical comparison intrinsics **lge**, **lgt**, **lle**,
  and **llt** differ from the corresponding intrinsic operators _.ge.,
  .gt., .le., and .lt._, in that the latter use the processor's character
  ordering (which is not ASCII on some targets), whereas the former
  always use the ASCII ordering.

### **Options**

- **string_a**
  : Shall be of default _character_ type.

- **string_b**
  : Shall be of default _character_ type.

### **Result**

  Returns _.true._ if string_a \> string_b, and _.false._ otherwise,
  based on the ASCII ordering.

  If both input arguments are null strings, _.false._ is always returned.

### **Examples**

Sample program:
```fortran
program demo_lgt
implicit none
integer :: i
   write(*,'(*(a))')(char(i),i=32,126)  ! ASCII order
   write(*,*) lgt('abc','ABC')          ! [T] lowercase is > uppercase
   write(*,*) lgt('abc','abc  ')        ! [F] trailing spaces
   ! If both strings are of zero length the result is false.
   write(*,*) lgt('','')                ! [F]
   write(*,*) lgt('','a')               ! [F] the null string is padded
   write(*,*) lgt('a','')               ! [T]
   write(*,*) lgt('abc',['abc','123'])  ! [F T]  scalar and array
   write(*,*) lgt(['cba', '123'],'abc') ! [T F]
   write(*,*) lgt(['abc','123'],['cba','123']) ! [F F]  both arrays
end program demo_lgt
```
Results:
```text
    !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
    [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
    T
    F
    F
    F
    T
    F T
    T F
    F F
```
### **Standard**

FORTRAN 77

### **See Also**

 [**lge**(3)](#lge),
 [**lle**(3)](#lle),
 [**llt**(3)](#llt)

  Functions that perform operations on character strings, return lengths
  of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),

[**scan**(3)](#scan),
[**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## lle

### **Name**

**lle**(3) - \[CHARACTER:COMPARE\] ASCII Lexical less than or equal

### **Synopsis**
```fortran
     result = lle(string_a, stringb)
```
```fortran
      elemental logical function lle(string_a, string_b)

       character(len=*),intent(in) :: string_a
       character(len=*),intent(in) :: string_b
```
### **Characteristics**

### **Description**

  **lle**(3) determines whether one string is lexically less than or equal
  to another string, where the two strings are interpreted as containing
  ASCII character codes. if the **string_a** and **string_b** are not
  the same length, the shorter is compared as if spaces were appended
  to it to form a value that has the same length as the longer. Leading
  spaces are significant.

  In general, the lexical comparison intrinsics **lge**, **lgt**, **lle**,
  and **llt** differ from the corresponding intrinsic operators _.ge.,
  .gt., .le., and .lt._, in that the latter use the processor's character
  ordering (which is not ASCII on some targets), whereas the former
  always use the ASCII ordering.

### **Options**

- **str_a**
  : variable or array of default _character_ type.

- **str_b**
  : variable or array of default _character_ type.

  if **str_a** and **str_b** are both arrays they must be of the
  same shape.

### **Result**

- **result**
  Returns _.true._ if **STR_A \<= STR_B**, and _.false._ otherwise, based on
  the ASCII ordering.

  If both input arguments are null strings, _.true._ is always returned.

### **Examples**

Sample program:

```fortran
program demo_lle
implicit none
integer :: i
   write(*,'(*(a))')(char(i),i=32,126)
   write(*,*) lle('abc','ABC')          ! F lowercase is > uppercase
   write(*,*) lle('abc','abc  ')        ! T trailing spaces
   ! If both strings are of zero length the result is true.
   write(*,*) lle('','')                ! T
   write(*,*) lle('','a')               ! T the null string is padded
   write(*,*) lle('a','')               ! F
   write(*,*) lle('abc',['abc','123'])  ! [T,F] scalar and array
   write(*,*) lle(['cba', '123'],'abc') ! [F,T]
   write(*,*) lle(['abc','123'],['cba','123']) ! [T,T] both arrays
end program demo_lle
```
Results:
```text
  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
  [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  F
  T
  T
  T
  F
  T F
  F T
  T T
```
### **Standard**

FORTRAN 77

### **See Also**

  [**lge**(3)](#lge),
  [**lgt**(3)](#lgt),
  [**llt**(3)](#llt)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),

[**scan**(3)](#scan),
[**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## llt

### **Name**

**llt**(3) - \[CHARACTER:COMPARE\] ASCII Lexical less than

### **Synopsis**
```fortran
     result = llt(string_a, stringb)
```
```fortran
      elemental logical function llt(string_a, string_b)

       character(len=*),intent(in) :: string_a
       character(len=*),intent(in) :: string_b
```
### **Characteristics**

### **Description**

  **llt**(3) determines whether one string is lexically less than
  another string, where the two strings are interpreted as containing
  ASCII character codes. If the **string_a** and **string_b** are not
  the same length, the shorter is compared as if spaces were appended
  to it to form a value that has the same length as the longer.

  In general, the lexical comparison intrinsics **lge**, **lgt**, **lle**,
  and **llt** differ from the corresponding intrinsic operators _.ge.,
  .gt., .le., and .lt._, in that the latter use the processor's character
  ordering (which is not ASCII on some targets), whereas the former
  always use the ASCII ordering.

### **Options**

- **string_a**
  : Shall be of default _character_ type.

- **string_b**
  : Shall be of default _character_ type.

### **Result**

  Returns _.true._ if string_a \<= string_b, and _.false._ otherwise,
  based on the ASCII ordering.

  If both input arguments are null strings, _.false._ is always returned.

### **Examples**

Sample program:
```fortran
program demo_llt
implicit none
integer :: i
   write(*,'(*(a))')(char(i),i=32,126)  ! ASCII order
   write(*,*) llt('abc','ABC')          ! [F] lowercase is > uppercase
   write(*,*) llt('abc','abc  ')        ! [F] trailing spaces
   ! If both strings are of zero length the result is false.
   write(*,*) llt('','')                ! [F]
   write(*,*) llt('','a')               ! [T] the null string is padded
   write(*,*) llt('a','')               ! [F]
   write(*,*) llt('abc',['abc','123'])  ! [F F]  scalar and array
   write(*,*) llt(['cba', '123'],'abc') ! [F T]
   write(*,*) llt(['abc','123'],['cba','123']) ! [T F]  both arrays
end program demo_llt
```
Results:
```text
  > !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
  > [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  > F
  > F
  > F
  > T
  > F
  > F F
  > F T
  > T F
```
### **Standard**

FORTRAN 77

### **See Also**

  [**lge**(3)](#lge),
  [**lgt**(3)](#lgt),
  [**lle**(3)](#lle))

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl), [**adjustr**(3)](#adjustr), [**index**(3)](#index),
  [**scan**(3)](#scan), [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## log10

### **Name**

**log10**(3) - \[MATHEMATICS\] Base 10 logarithm function

### **Synopsis**
```fortran
    result = log10(x)
```
```fortran
     elemental real(kind=KIND) function log10(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** may be any kind of _real_ value
 - the result is the same type and characteristics as **x**.

### **Description**

  **log10**(3) computes the base 10 logarithm of **x**. This is generally
  called the "common logarithm".

### **Options**

- **x**
  : A _real_ value > 0 to take the log of.

### **Result**

  The logarithm to base 10 of **x**

### **Examples**

Sample program:
```fortran
program demo_log10
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 10.0_real64

   x = log10(x)
   write(*,'(*(g0))')'log10(',x,') is ',log10(x)

   ! elemental
   write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &
                     & 100000.0, 1000000.0, 10000000.0])

end program demo_log10
```
Results:
```text
   log10(1.0000000000000000) is 0.0000000000000000
      0.00000000       1.00000000       2.00000000       3.00000000
      4.00000000       5.00000000       6.00000000       7.00000000
```
### **Standard**

FORTRAN 77

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## log_gamma

### **Name**

**log_gamma**(3) - \[MATHEMATICS\] Logarithm of the absolute value of
the Gamma function

### **Synopsis**
```fortran
    result = log_gamma(x)
```
```fortran
     elemental real(kind=KIND) function log_gamma(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

  - **x** may be any _real_ type
  - the return value is of same type and kind as **x**.

### **Description**

  **log_gamma**(3) computes the natural logarithm of the absolute value
  of the Gamma function.

### **Options**

- **x**
  : A non-negative (neither negative nor zero) value to render the
  result for.

### **Result**

 The result has a value equal to a processor-dependent approximation
 to the natural logarithm of the absolute value of the gamma function
 of **x**.

### **Examples**

Sample program:
```fortran
program demo_log_gamma
implicit none
real :: x = 1.0
   write(*,*)x,log_gamma(x) ! returns 0.0
   write(*,*)x,log_gamma(3.0) ! returns 0.693 (approximately)
end program demo_log_gamma
```
Results:
```text
      1.000000      0.0000000E+00
      1.000000      0.6931472
```
### **Standard**

Fortran 2008

### **See Also**

Gamma function: [**gamma**(3)](#gamma)

 _fortran-lang intrinsic descriptions_

## logical

### **Name**

**logical**(3) - \[TYPE:LOGICAL\] Converts one kind of _logical_ variable to another

### **Synopsis**
```fortran
    result = logical(l [,kind])
```
```fortran
     elemental logical(kind=KIND) function logical(l,kind)

      logical(kind=KIND),intent(in) :: l
      integer(kind=**),intent(in),optional :: kind
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

### **Description**

  **logical**(3) converts one kind of _logical_ variable to another.

### **Options**

- **l**
  : The type shall be _logical_.

- **kind**
  : An _integer_ initialization expression indicating the kind parameter
  of the result. If not present, the default kind is returned.

### **Result**

The return value is a _logical_ value equal to **l**, with a kind
corresponding to **kind**, or of the default logical kind if **kind**
is not given.

### **Examples**

Sample program:
```fortran
program demo_logical
! Access array containing the kind type parameter values supported by this
! compiler for entities of logical type
use iso_fortran_env, only : logical_kinds

   ! list kind values supported on this platform, which generally vary
   ! in storage size
   do i =1, size(logical_kinds)
      write(*,*)logical_kinds(i)
   enddo

end program demo_logical
```
Results:
```text
              1
              2
              4
              8
             16
```
### **Standard**

Fortran 95 , related ISO_FORTRAN_ENV module - fortran 2009

### **See Also**

[**int**(3)](#int),
[**real**(3)](#real),
[**cmplx**(3)](#cmplx)

 _fortran-lang intrinsic descriptions_

## log

### **Name**

**log**(3) - \[MATHEMATICS\] Logarithm function

### **Synopsis**
```fortran
  result = log(x)
```
```fortran
   elemental TYPE(kind=KIND) function log(x)

    TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** may be any kind of _real_ or _complex_ value
 - the result is the same type and characteristics as **x**.

### **Description**

  **log**(3) computes the natural logarithm of **x**, i.e. the logarithm to
  the base "e".

### **Options**

- **x**
  : The value to take the log of

### **Result**

  The natural logarithm of **xx**.
  If **x** is _complex_, the imaginary part OMEGA is in the range
```fortran
    -PI < OMEGA <= PI
```

### **Examples**

Sample program:
```fortran
program demo_log
implicit none
  real(kind(0.0d0)) :: x = 2.71828182845904518d0
  complex :: z = (1.0, 2.0)
  write(*,*)x, log(x)    ! will yield (approximately) 1
  write(*,*)z, log(z)
end program demo_log
```
Results:
```text
      2.7182818284590451        1.0000000000000000
   (1.00000000,2.00000000) (0.804718971,1.10714877)
```
### **Standard**

FORTRAN 77

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## maskl

### **Name**

**maskl**(3) - \[BIT:SET\] Generates a left justified mask

### **Synopsis**
```fortran
    result = maskl( i [,kind] )
```
```fortran
     elemental integer(kind=KIND) function maskl(i,kind)

      integer(kind=**),intent(in) :: i
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **kind** Shall be a scalar constant expression of type _integer_
  whose value is a supported _integer_ kind.
- The result is of the same _kind_ as **i** unless **kind** is
  present, which is then used to specify the kind of the result.

### **Description**

  **maskl**(3) has its leftmost **i** bits set to **1**, and the
  remaining bits set to **0**.

### **Options**

- **i**
  : the number of left-most bits to set in the _integer_ result. It
  must be from 0 to the number of bits for the kind of the result.
  The default kind of the result is the same as **i** unless the result
  size is specified by **kind**. That is, these Fortran statements must
  be _.true._ :
```fortran
   i >= 0 .and. i < bitsize(i) ! if KIND is not specified
   i >= 0 .and. i < bitsize(0_KIND) ! if KIND is specified
```
- **kind**
  : designates the kind of the _integer_ result.

### **Result**

  The leftmost **i** bits of the output _integer_ are set to 1 and the
  other bits are set to 0.

### **Examples**

Sample program:
```fortran
program demo_maskl
implicit none
integer :: i
   i=maskl(1)
   write(*,'(i0,1x,b0,/)') i,i
   ! elemental
   write(*,'(*(i11,1x,b0.32,1x,/))') maskl([(i,i,i=0,bit_size(0),4)])
end program demo_maskl
```
Results:
```text
   -2147483648 10000000000000000000000000000000

             0 00000000000000000000000000000000
    -268435456 11110000000000000000000000000000
     -16777216 11111111000000000000000000000000
      -1048576 11111111111100000000000000000000
        -65536 11111111111111110000000000000000
         -4096 11111111111111111111000000000000
          -256 11111111111111111111111100000000
           -16 11111111111111111111111111110000
            -1 11111111111111111111111111111111
```
Results:
```text
```
### **Standard**

Fortran 2008

### **See Also**

[**maskr**(3)](#maskr)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## maskr

### **Name**

**maskr**(3) - \[BIT:SET\] Generates a right-justified mask

### **Synopsis**
```fortran
    result = maskr( i [,kind] )
```
```fortran
     elemental integer(kind=KIND) function maskr(i,kind)

      integer(kind=**),intent(in) :: i
      integer(kind=**),intent(in),optional :: kind
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **kind** Shall be a scalar constant expression of type _integer_
  whose value is a supported _integer_ kind.
- The result is of the same _kind_ as **i** unless **kind** is
  present, which is then used to specify the kind of the result.

### **Description**

  **maskr**(3) the result is an _integer_ with its rightmost **i**
  bits set to 1, and the remaining bits set to 0.

### **Options**

- **i**
  : the number of right-most bits to set in the _integer_ result. It
  must be from 0 to the number of bits for the kind of the result.
  The default kind of the result is the same as **i** unless the result
  size is specified by **kind**. That is, these Fortran statements must
  be _.true._ :
```fortran
   i >= 0 .and. i < bitsize(i) ! if KIND is not specified
   i >= 0 .and. i < bitsize(0_KIND) ! if KIND is specified
```
- **kind**
  : designates the kind of the _integer_ result.

### **Result**

  The rightmost **i** bits of the output _integer_ are set to 1 and the
  other bits are set to 0.

### **Examples**

Sample program:
```fortrqn
program demo_maskr
implicit none
integer :: i
  ! basics
   i=maskr(1)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-1)
   i=maskr(5)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-5)
   i=maskr(11)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-11)
  ! elemental
   write(*,'(*(i11,1x,b0.32,1x,/))') maskr([(i,i,i=0,bit_size(0),4)])
end program demo_maskr
```
Results:
```text
   1 1 10000000000000000000000000000000

   31 11111 111000000000000000000000000000

   2047 11111111111 111000000000000000000000

             0 00000000000000000000000000000000
            15 00000000000000000000000000001111
           255 00000000000000000000000011111111
          4095 00000000000000000000111111111111
         65535 00000000000000001111111111111111
       1048575 00000000000011111111111111111111
      16777215 00000000111111111111111111111111
     268435455 00001111111111111111111111111111
            -1 11111111111111111111111111111111
```
### **Standard**

Fortran 2008

### **See Also**

[**maskl**(3)](#maskl)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## matmul

### **Name**

**matmul**(3) - \[TRANSFORMATIONAL\] Numeric or logical matrix
multiplication

### **Synopsis**
```fortran
    result = matmul(matrix_a,matrix_b)
```
```fortran
     function matmul(matrix_a, matrix_b)

      type(NUMERIC_OR_LOGICAL) :: matrix_a(..)
      type(NUMERIC_OR_LOGICAL) :: matrix_b(..)
      type(PROMOTED) :: matmul(..)
```
### **Characteristics**

 - Arguments may be numeric (_integer_, _real_, or _complex_ )
   or _logical_ and must be one or two-dimensional arrays.
 - At least one argument must be rank two.
 - If one argument is _logical_, both must be _logical_.

### **Description**

 **matmul**(3) performs a matrix multiplication on numeric or logical
 arguments.

### **Options**

- **matrix_a**
  : A numeric or logical array with a rank of one or two.

- **matrix_b**
  : A numeric or logical array with a rank of one or two. The last
  dimension of **matrix_a** and the first dimension of **matrix_b**
  must be equal.

  Note that **matrix_a** and **matrix_b** may be different numeric
  types.

### **Result**

####  **Numeric Arguments**

  If **matrix_a** and **matrix_b** are numeric the result is an
  array containing the conventional matrix product of **matrix_a**
  and **matrix_b**.

  First, for the numeric expression **C=matmul(A,B)**

   - Any vector **A(n)** is treated as a row vector **A(1,n)**.
   - Any vector **B(n)** is treated as a column vector **B(n,1)**.

#####  **Shape and Rank**

  The shape of the result can then be determined as the number of rows
  of the first matrix and the number of columns of the second; but if
  any argument is of rank one (a vector) the result is also rank one.
  Conversely when both arguments are of rank two, the result has a rank
  of two. That is ...

   + If **matrix_a** has shape [n,m] and **matrix_b** has shape [m,k],
     the result has shape [n,k].
   + If **matrix_a** has shape [m] and **matrix_b** has shape [m,k],
     the result has shape [k].
   + If **matrix_a** has shape [n,m] and **matrix_b** has shape [m],
     the result has shape [n].

#####  **Values**

  Then element **C(i,j)** of the product is obtained by multiplying
  term-by-term the entries of the ith row of **A** and the jth column
  of **B**, and summing these products. In other words, **C(i,j)**
  is the dot product of the ith row of **A** and the jth column of **B**.

#####  **Characteristics**

  The returned array will be promoted to the same type and kind as would
  result from multiplication between an element of each argument (like
  the multiplication operator (\*) had been used between the elements).

#### **Logical Arguments**

#####  **Values**

  If **matrix_a** and **matrix_b** are of type logical, the array elements
  of the result are instead:
```fortran
  Value_of_Element (i,j) = &
  ANY( (row_i_of_MATRIX_A) .AND. (column_j_of_MATRIX_B) )
```
#####  **Characteristics**

  The returned array is of the type and kind that results if any element of
  each argument had been operated on by the **.AND.** operator.

### **Examples**

Sample program:
```fortran
program demo_matmul
implicit none
integer :: a(2,3), b(3,2), c(2), d(3), e(2,2), f(3), g(2), v1(4),v2(4)
   a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
   b = reshape([10, 20, 30, 40, 50, 60], [3, 2])
   c = [1, 2]
   d = [1, 2, 3]
   e = matmul(a, b)
   f = matmul(c,a)
   g = matmul(a,d)

   call print_matrix_int('A is ',a)
   call print_matrix_int('B is ',b)
   call print_vector_int('C is ',c)
   call print_vector_int('D is ',d)
   call print_matrix_int('E is matmul(A,B)',e)
   call print_vector_int('F is matmul(C,A)',f)
   call print_vector_int('G is matmul(A,D)',g)

   ! look at argument shapes when one is a vector
   write(*,'(" > shape")')
   ! at least one argument must be of rank two
   ! so for two vectors at least one must be reshaped
   v1=[11,22,33,44]
   v2=[10,20,30,40]

   ! these return a vector C(1:1)
   ! treat A(1:n) as A(1:1,1:n)
   call print_vector_int('Cd is a vector (not a scalar)',&
   & matmul(reshape(v1,[1,size(v1)]),v2))
   ! or treat B(1:m) as B(1:m,1:1)
   call print_vector_int('cD is a vector too',&
   & matmul(v1,reshape(v2,[size(v2),1])))

   ! or treat A(1:n) as A(1:1,1:n) and B(1:m) as B(1:m,1:1)
   ! but note this returns a matrix C(1:1,1:1) not a vector!
   call print_matrix_int('CD is a matrix',matmul(&
   & reshape(v1,[1,size(v1)]), &
   & reshape(v2,[size(v2),1])))

contains

! CONVENIENCE ROUTINES TO PRINT IN ROW-COLUMN ORDER
subroutine print_vector_int(title,arr)
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:)
   call print_matrix_int(title,reshape(arr,[1,shape(arr)]))
end subroutine print_vector_int

subroutine print_matrix_int(title,arr)
!@(#) print small 2d integer arrays in row-column format
character(len=*),parameter :: all='(" > ",*(g0,1x))' ! a handy format
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest

   print all
   print all, trim(title)
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo

end subroutine print_matrix_int

end program demo_matmul
```
Results:
```text
    >
    > A is
    > [  1,  3,  5 ]
    > [  2,  4,  6 ]
    >
    > B is
    > [  10,  40 ]
    > [  20,  50 ]
    > [  30,  60 ]
    >
    > C is
    > [  1,  2 ]
    >
    > D is
    > [  1,  2,  3 ]
    >
    > E is matmul(A,B)
    > [  220,  490 ]
    > [  280,  640 ]
    >
    > F is matmul(C,A)
    > [   5,  11,  17 ]
    >
    > G is matmul(A,D)
    > [  22,  28 ]
    > shape
    >
    > Cd is a vector (not a scalar)
    > [  3300 ]
    >
    > cD is a vector too
    > [  3300 ]
    >
    > CD is a matrix
    > [  3300 ]
```
### **Standard**

Fortran 95

### **See Also**

[**product**(3)](#product),
[**transpose**(3)](#transpose)

### **Resources**

- [Matrix multiplication : Wikipedia](https://en.wikipedia.org/wiki/Matrix_multiplication)
- The Winograd variant of Strassen's matrix-matrix multiply algorithm may
  be of interest for optimizing multiplication of very large matrices. See
```text
    "GEMMW: A portable level 3 BLAS Winograd variant of Strassen's
    matrix-matrix multiply algorithm",

    Douglas, C. C., Heroux, M., Slishman, G., and Smith, R. M.,
    Journal of Computational Physics,
    Vol. 110, No. 1, January 1994, pages 1-10.

  The numerical instabilities of Strassen's method for matrix
  multiplication requires special processing.
```
 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## maxexponent

### **Name**

**maxexponent**(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind

### **Synopsis**
```fortran
    result = maxexponent(x)
```
```fortran
     elemental integer function maxexponent(x)

      real(kind=KIND),intent(in)   :: x
```
### **Characteristics**

where KIND is any _real_ kind.

### **Description**

**maxexponent**(3) returns the maximum exponent in the model of the type
of **x**.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The return value is of type _integer_ and of the default integer kind.

### **Examples**

Sample program:
```fortran
program demo_maxexponent
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x
real(kind=dp) :: y

   print *, minexponent(x), maxexponent(x)
   print *, minexponent(y), maxexponent(y)
end program demo_maxexponent
```
Results:
```text
           -125         128
          -1021        1024
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
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

 _fortran-lang intrinsic descriptions_

## maxloc

### **Name**

**maxloc**(3) - \[ARRAY:LOCATION\] Location of the maximum value within an array

### **Synopsis**
```fortran
    result = maxloc(array [,mask])
```
```fortran
     NUMERIC function maxloc(array, mask)

      NUMERIC,intent(in) :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
or
```fortran
    result = maxloc(array [,dim] [,mask])
```
```fortran
     NUMERIC function maxloc(array, dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type
  - **NUMERIC** designates any intrinsic numeric type and kind.

### **Description**

**maxloc**(3) determines the location of the element in the array with
the maximum value, or, if the **dim** argument is supplied, determines
the locations of the maximum element along each row of the array in the
**dim** direction.

If **mask** is present, only the elements for which **mask**
is _.true._ are considered. If more than one element in the array has
the maximum value, the location returned is that of the first such element
in array element order.

If the array has zero size, or all of the elements
of **mask** are .false., then the result is an array of zeroes. Similarly,
if **dim** is supplied and all of the elements of **mask** along a given
row are zero, the result value for that row is zero.

### **Options**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : Shall be an array of type _logical_, and conformable with **array**.

### **Result**

If **dim** is absent, the result is a rank-one array with a length equal
to the rank of **array**. If **dim** is present, the result is an array
with a rank one less than the rank of **array**, and a size corresponding
to the size of **array** with the **dim** dimension removed. If **dim**
is present and **array** has a rank of one, the result is a scalar. In
all cases, the result is of default _integer_ type.

The value returned is reference to the offset from the beginning of the
array, not necessarily the subscript value if the array subscripts do
not start with one.

### **Examples**

sample program

```fortran
program demo_maxloc
implicit none
integer      :: ii
integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
   10, 20, 30, 40, 50, &
   11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

    write(*,*) maxloc(ints)
    write(*,*) maxloc(ints,dim=1)
    write(*,*) maxloc(ints,dim=2)
    ! when array bounds do not start with one remember MAXLOC(3) returns
    ! the offset relative to the lower bound-1 of the location of the
    ! maximum value, not the subscript of the maximum value. When the
    ! lower bound of the array is one, these values are the same. In
    ! other words, MAXLOC(3) returns the subscript of the value assuming
    ! the first subscript of the array is one no matter what the lower
    ! bound of the subscript actually is.
    write(*,'(g0,1x,g0)') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))
    write(*,*)maxloc(i)

end program demo_maxloc
```

Results:

```text
      3       5
      3       3       3       3       3
      5       5       5
   -3 47
   -2 48
   -1 49
   0 50
   1 49
   2 48
   3 47
```

### **Standard**

Fortran 95

### **See Also**

[**maxloc**(3)](#maxloc),
[**minloc**(3)](#minloc),
[**maxval**(3)](#maxval),
[**minval**(3)](#minval),
[**max**(3)](#max)

 _fortran-lang intrinsic descriptions_

## max

### **Name**

**max**(3) - \[NUMERIC\] Maximum value of an argument list

### **Synopsis**
```fortran
    result = max(a1, a2, a3, ...)
```
```fortran
     elemental TYPE(kind=kind(a1)) function max(a1, a2, a3, ... )

      TYPE(kind=kind(a1),intent(in),optional :: a1
      TYPE(kind=kind(a1),intent(in),optional :: a2
      TYPE(kind=kind(a1),intent(in),optional :: a3
                :
                :
                :
```
### **Characteristics**

- **TYPE** may be _integer_ or _real_
- **a1, a2, a3, ...** must be of the same type and kind as **a1**.

### **Description**

**max**(3) returns the argument with the largest (most positive) value.

### **Options**

- **a1**
  : The first argument determines the type and kind of the returned
  value, and of any remaining arguments.

- **a2,a3,...**
  : A value or expression of the same type and kind as **a1**.
  There must be at least two arguments to **max(3)**.

### **Result**

  The return value corresponds to the maximum value among the arguments.

  The function is both elemental and allows for an arbitrary number of
  arguments. This means if some elements are scalar and some are arrays
  that all the arrays must be of the same size, and the returned value
  will be an array that is the result as if multiple calls were made
  with all scalar values with a single element of each array used in
  each call. If called with all arrays the returned array is the same
  as if multiple calls were made with **max(arr1(1),arr2(1), ...)**
  to **max(arr1(N),arr2(N))**.

### **Examples**

Sample program
```fortran
program demo_max
implicit none
real :: arr1(4)= [10.0,11.0,30.0,-100.0]
real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]

  ! basic usage
   ! this is simple enough because all arguments are scalar
   write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)
   ! this is all max(3) could do before it became an elemental
   ! function and is the most intuitive
   ! except that it can take an arbitrary number of options,
   ! which is not common in Fortran without
   ! declaring a lot of optional parameters.
   !
   ! That is it unless you want to use the elemental features of max(3)!

  ! elemental
   ! there must be at least two arguments, so even if A1 is an array
   ! max(A1) is not valid. See MAXVAL(3) and/or MAXVAL(3) instead.

   ! If any argument is an array by the definition of an elemental
   ! function all the array arguments must be the same shape but
   ! MAXVAL([arr1, arr2]) or max(maxval(arr1),maxval(arr2))
   ! would work, for example.

   ! so an elemental call of two vectors does not return a single
   ! value, but the largest first element of the arrays, then the
   ! largest second element, and so on.
   write(*,*)max(arr1,arr2(1:4))
   ! multi-dimensional arrays are allowed, where the returned
   ! value will be an array of all the sets of the elements with
   ! the same coordinates.

   ! When mixing arrays and scalars you can think of the scalars
   ! as being a copy of one of the arrays with all values set to
   ! the scalar value ...
   write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2)

   ! with two arrays and some scalars ...
   write(*,*)'scalars and array:',&
   & max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)
end program demo_max
```
Results:
```text
    scalars:   30.000000
      20.0000000  21.000000  32.000000 -100.00000
    scalars and array: 30.000000 30.000000 32.000000 30.000000 2200.0000
    scalars and array: 40.000000 40.000000 40.000000 40.000000
```
### **Standard**

FORTRAN 77

### **See Also**

[**maxloc**(3)](#maxloc),
[**minloc**(3)](#minloc),
[**maxval**(3)](#maxval),
[**minval**(3)](#minval),
[**min**(3)](#min)

 _fortran-lang intrinsic descriptions_

## maxval

### **Name**

**maxval**(3) - \[ARRAY REDUCTION\] Determines the maximum value in an array or row

### **Synopsis**
```fortran
    result = maxval(array [,mask])
```
```fortran
     NUMERIC function maxval(array ,mask)

      NUMERIC,intent(in) :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
or
```fortran
    result = maxval(array [,dim] [,mask])
```
```fortran
     NUMERIC function maxval(array ,dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

 - **NUMERIC** designates any numeric type and kind.

### **Description**

**maxval**(3) determines the maximum value of the elements in an
array value, or, if the **dim** argument is supplied, determines the
maximum value along each row of the array in the **dim** direction. If
**mask** is present, only the elements for which **mask** is _.true._
are considered. If the array has zero size, or all of the elements of
**mask** are _.false._, then the result is the most negative number of
the type and kind of **array** if **array** is numeric, or a string of
nulls if **array** is of character type.

### **Options**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : (Optional) Shall be an array of type _logical_, and conformable with
  **array**.

### **Result**

If **dim** is absent, or if **array** has a rank of one, the result is a scalar.
If **dim** is present, the result is an array with a rank one less than the
rank of **array**, and a size corresponding to the size of **array** with the
**dim** dimension removed. In all cases, the result is of the same type and
kind as **array**.

### **Examples**

sample program:

```fortran
program demo_maxval
implicit none
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
  10, 20, 30, 40, 50, &
  11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

   write(*,*) maxval(ints)
   write(*,*) maxval(ints,dim=1)
   write(*,*) maxval(ints,dim=2)
   ! find biggest number less than 30 with mask
   write(*,*) maxval(ints,mask=ints.lt.30)
end program demo_maxval
```
Results:
```
   55
   11     22     33     44     55
    5     50     55
   22
```
### **Standard**

Fortran 95

### **See Also**

[**maxloc**(3)](#maxloc),
[**minloc**(3)](#minloc),
[**minval**(3)](#minval),
[**max**(3)](#max),
[**min**(3)](#min)

 _fortran-lang intrinsic descriptions_

## merge_bits

### **Name**

**merge_bits**(3) - \[BIT:COPY\] Merge bits using a mask

### **Synopsis**
```fortran
    result = merge_bits(i, j, mask)
```
```fortran
     elemental integer(kind=KIND) function merge_bits(i,j,mask)

      integer(kind=KIND), intent(in) :: i, j, mask
```
### **Characteristics**

where the result and all input values have the same _integer_ type and
KIND with the exception that the mask and either **i** or **j** may be
a BOZ constant.

### **Description**

A common graphics operation in Ternary Raster Operations is to combine
bits from two different sources, generally referred to as bit-blending.
**merge_bits**(3) performs a masked bit-blend of **i** and **j** using
the bits of the **mask** value to determine which of the input values
to copy bits from.

Specifically, The k-th bit of the result is equal to the k-th bit of
**i** if the k-th bit of **mask** is **1**; it is equal to the k-th bit
of **j** otherwise (so all three input values must have the same number
of bits).

The resulting value is the same as would result from
```fortran
    ior (iand (i, mask),iand (j, not (mask)))
```
An exception to all values being of the same _integer_ type is that **i**
or **j** and/or the mask may be a BOZ constant (A BOZ constant means it is
either a Binary, Octal, or Hexadecimal literal constant). The BOZ values
are converted to the _integer_ type of the non-BOZ value(s) as if called
by the intrinsic function **int()** with the kind of the non-BOZ value(s),
so the BOZ values must be in the range of the type of the result.

### **Options**

- **i**
  : value to select bits from when the associated bit in the mask is **1**.

- **j**
  : value to select bits from when the associated bit in the mask is **0**.

- **mask**
  : a value whose bits are used as a mask to select bits from **i** and **j**

### **Result**

The bits blended from **i** and **j** using the mask **mask**. It is the
same type as **i** if **i** is of type _integer_, otherwise the same type
as **j**.

### **Examples**

Sample program:
```fortran
program demo_merge_bits
use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
implicit none
integer(kind=int16) :: if_one,if_zero,msk
character(len=*),parameter :: fmt='(*(g0, 1X))'

   ! basic usage
   print *,'MERGE_BITS( 5,10,41) should be 3.=>',merge_bits(5,10,41)
   print *,'MERGE_BITS(13,18,22) should be 4.=>',merge_bits(13,18,22)

   ! use some values in base2 illustratively:
   if_one =int(b'1010101010101010',kind=int16)
   if_zero=int(b'0101010101010101',kind=int16)

   msk=int(b'0101010101010101',kind=int16)
   print '("should get all zero bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk)

   msk=int(b'1010101010101010',kind=int16)
   print '("should get all ones bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk)

   ! using BOZ values
   print fmt, &
   & merge_bits(32767_int16,    o'12345',         32767_int16), &
   & merge_bits(o'12345', 32767_int16, b'0000000000010101'), &
   & merge_bits(32767_int16,    o'12345',             z'1234')

   ! a do-it-yourself equivalent for comparison and validation
   print fmt, &
   & ior(iand(32767_int16, 32767_int16),                   &
   &   iand(o'12345', not(32767_int16))),                  &

   & ior(iand(o'12345', int(o'12345', kind=int16)),        &
   &   iand(32767_int16, not(int(o'12345', kind=int16)))), &

   & ior(iand(32767_int16, z'1234'),                       &
   &   iand(o'12345', not(int( z'1234', kind=int16))))

end program demo_merge_bits
```
Results:
```text
    MERGE_BITS( 5,10,41) should be 3.=>           3
    MERGE_BITS(13,18,22) should be 4.=>           4
   should get all zero bits =>0000000000000000
   should get all ones bits =>1111111111111111
   32767 32751 5877
   32767 32767 5877
```
### **Standard**

Fortran 2008

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## merge

### **Name**

**merge**(3) - \[ARRAY CONSTRUCTION\] Merge variables

### **Synopsis**
```fortran
    result = merge(tsource, fsource, mask)
```
```fortran
     elemental type(TYPE(kind=KIND)) function merge(tsource,fsource,mask)

      type(TYPE(kind=KIND)),intent(in) :: tsource
      type(TYPE(kind=KIND)),intent(in) :: fsource
      logical(kind=**),intent(in)   :: mask
      mask** : Shall be of type _logical_.
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
 - **tsource** May be of any type, including user-defined.
 - **fsource** Shall be of the same type and type parameters as **tsource**.
 - The result will by of the same type and type parameters as **tsource** too.

### **Description**

The elemental function **merge**(3) selects values from two arrays or
scalars according to a logical mask. The result is equal to an element
of **tsource** where the corresponding element of **mask** is _.true._, or an
element of **fsource** when it is _.false._ .

Multi-dimensional arrays are supported.

Note that argument expressions to **merge**(3) are not required to be
short-circuited so (as an example) if the array **x** contains zero values
in the statement below the standard does not prevent floating point
divide by zero being generated; as **1.0/x** may be evaluated for all values
of **x** before the mask is used to select which value to retain:

```fortran
      y = merge( 1.0/x, 0.0, x /= 0.0 )
```

Note the compiler is also free to short-circuit or to generate an
infinity so this may work in many programming environments but is not
recommended.

For cases like this one may instead use masked assignment via the **where**
construct:

```fortran
      where(x .ne. 0.0)
         y = 1.0/x
      elsewhere
         y = 0.0
      endwhere
```

instead of the more obscure

```fortran
      merge(1.0/merge(x,1.0,x /= 0.0), 0.0, x /= 0.0)
```
### **Options**

- **tsource**
  : May be of any type, including user-defined.

- **fsource**
  : Shall be of the same type and type parameters as **tsource**.

- **mask**
  : Shall be of type _logical_.

Note that (currently) _character_ values must be of the same length.

### **Result**

The result is of the same type and type parameters as **tsource**. For any
element the result is **tsource** if **mask** is true and **fsource** otherwise.

### **Examples**

Sample program:
```fortran
program demo_merge
implicit none
integer :: tvals(2,3), fvals(2,3), answer(2,3)
logical :: mask(2,3)
integer :: i
integer :: k
logical :: chooseleft

   ! Works with scalars
   k=5
   write(*,*)merge (1.0, 0.0, k > 0)
   k=-2
   write(*,*)merge (1.0, 0.0, k > 0)

   ! set up some simple arrays that all conform to the
   ! same shape
   tvals(1,:)=[  10, -60,  50 ]
   tvals(2,:)=[ -20,  40, -60 ]

   fvals(1,:)=[ 0, 3, 2 ]
   fvals(2,:)=[ 7, 4, 8 ]

   mask(1,:)=[ .true.,  .false., .true. ]
   mask(2,:)=[ .false., .false., .true. ]

   ! lets use the mask of specific values
   write(*,*)'mask of logicals'
   answer=merge( tvals, fvals, mask )
   call printme()

   ! more typically the mask is an expression
   write(*, *)'highest values'
   answer=merge( tvals, fvals, tvals > fvals )
   call printme()

   write(*, *)'lowest values'
   answer=merge( tvals, fvals, tvals < fvals )
   call printme()

   write(*, *)'zero out negative values'
   answer=merge( tvals, 0, tvals < 0)
   call printme()

   write(*, *)'binary choice'
   chooseleft=.false.
   write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)
   chooseleft=.true.
   write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)

contains

subroutine printme()
      write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))
end subroutine printme

end program demo_merge
```

Expected Results:

```
    mask of logicals
     10   3  50
      7   4 -60
    highest values
     10   3  50
      7  40   8
    lowest values
      0 -60   2
    -20   4 -60
    zero out negative values
      0 -60   0
    -20   0 -60
    binary choice
     10  20  30
      1   2   3
```

### **Standard**

Fortran 95

### **See Also**

- [**pack**(3)](#pack) packs an array into an array of rank one
- [**spread**(3)](#spread) is used to add a dimension and replicate data
- [**unpack**(3)](#unpack) scatters the elements of a vector
- [**transpose**(3)](#transpose) - Transpose an array of rank two

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## minexponent

### **Name**

**minexponent**(3) - \[NUMERIC MODEL\] Minimum exponent of a real kind

### **Synopsis**
```fortran
    result = minexponent(x)
```
```fortran
     elemental integer function minexponent(x)

      real(kind=KIND),intent(in)   :: x
```
### **Characteristics**

where KIND is any _real_ kind.

### **Description**

**minexponent**(3) returns the minimum exponent in the model of the type
of **x**.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The return value is of type _integer_ and of the default integer kind.

### **Examples**

Sample program:

```fortran
program demo_minexponent
use, intrinsic :: iso_fortran_env, only : &
 &real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x
real(kind=real64) :: y
    print *, minexponent(x), maxexponent(x)
    print *, minexponent(y), maxexponent(y)
end program demo_minexponent
```
Expected Results:
```
        -125         128
       -1021        1024
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## minloc

### **Name**

**minloc**(3) - \[ARRAY:LOCATION\] Location of the minimum value within an array

### **Synopsis**
```fortran
    result = minloc(array [,mask])
```
```fortran
     NUMERIC function minloc(array, mask)

      NUMERIC,intent(in) :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
or
```fortran
    result = minloc(array [,dim] [,mask])
```
```fortran
     NUMERIC function minloc(array, dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type
  - **NUMERIC** is any numeric type and kind.

### **Description**

  **minloc**(3) determines the location of the element in the array with
  the minimum value, or, if the **dim** argument is supplied, determines
  the locations of the minimum element along each row of the array in
  the **dim** direction. If **mask** is present, only the elements for
  which **mask** is _.true._ are considered. If more than one element in
  the array has the minimum value, the location returned is that of the
  first such element in array element order. If the array has zero size,
  or all of the elements of **mask** are _.false._, then the result is
  an array of zeroes. Similarly, if **dim** is supplied and all of the
  elements of **mask** along a given row are zero, the result value for
  that row is zero.

### **Options**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : Shall be an array of type _logical_, and conformable with **array**.

### **Result**

If **dim** is absent, the result is a rank-one array with a length equal
to the rank of **array**. If **dim** is present, the result is an array
with a rank one less than the rank of **array**, and a size corresponding
to the size of **array** with the **dim** dimension removed. If **dim**
is present and **array** has a rank of one, the result is a scalar. In
all cases, the result is of default _integer_ type.

### **Examples**

sample program:

```fortran
program demo_minloc
implicit none
integer,save :: ints(3,5)= reshape([&
   4, 10,  1,  7, 13, &
   9, 15,  6, 12,  3, &
  14,  5, 11,  2,  8  &
],shape(ints),order=[2,1])
   write(*,*) minloc(ints)
   write(*,*) minloc(ints,dim=1)
   write(*,*) minloc(ints,dim=2)
   ! where in each column is the smallest number .gt. 10 ?
   write(*,*) minloc(ints,dim=2,mask=ints.gt.10)
   ! a one-dimensional array with dim=1 explicitly listed returns a scalar
   write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar
end program demo_minloc
```
Results:
```text
         1       3
         1       3       1       3       2
         3       5       4
         5       4       3
         7
```
### **Standard**

Fortran 95

### **See Also**

[**min**(3)](#min),
[**maxloc**(3)](#maxloc),
[**minval**(3)](#minval),
[**maxval**(3)](#maxval),
[**max**(3)](#max)

 _fortran-lang intrinsic descriptions_

## min

### **Name**

**min**(3) - \[NUMERIC\] Minimum value of an argument list

### **Synopsis**
```fortran
    result = min(a1, a2, a3, ... )
```
```fortran
     elemental TYPE(kind=kind(a1)) function min(a1, a2, a3, ... )

      TYPE(kind=kind(a1),intent(in)   :: a1
      TYPE(kind=kind(a1),intent(in)   :: a2
      TYPE(kind=kind(a1),intent(in)   :: a3
                :
                :
                :
```
### **Characteristics**

Where **TYPE** may be _integer_ or _real_

### **Description**

**min**(3) returns the argument with the smallest (most negative) value.

### **Options**

- **a1**
  : The type shall be _integer_ or _real_.

- **a2, a3, ...**
  : An expression of the same type and kind as **a1**.

### **Result**

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

### **Examples**

Sample program
```fortran
program demo_min
implicit none
    write(*,*)min(10.0,11.0,30.0,-100.0)
end program demo_min
```
Results:
```
      -100.0000000
```
### **Standard**

FORTRAN 77

### **See Also**

[**maxloc**(3)](#maxloc),
[**minloc**(3)](#minloc),
[**minval**(3)](#minval),
[**max**(3)](#max),

 _fortran-lang intrinsic descriptions_

## minval

### **Name**

**minval**(3) - \[ARRAY REDUCTION\] Minimum value of an array

### **Synopsis**
```fortran
    result = minval(array, [mask])
```
```fortran
     NUMERIC function minval(array ,mask)

      NUMERIC,intent(in) :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
  or
```fortran
    result = minval(array [,dim] [,mask])
```
```fortran
     NUMERIC function minval(array, dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

 - **NUMERIC** is any numeric type and kind.

### **Description**

  **minval**(3) determines the minimum value of the elements in an array
  value, or, if the **dim** argument is supplied, determines the minimum
  value along each row of the array in the **dim** direction.

  If **mask** is present, only the elements for which **mask** is
  _.true._ are considered.

  If the array has zero size, or all of the elements of **mask**
  are _.false._, then the result is **huge(array)** if **array** is
  numeric, or a string of **char(len=255)** characters if **array**
  is of character type.

### **Options**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of ARRAY, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : Shall be an array of type _logical_, and conformable with **array**.

### **Result**

If **dim** is absent, or if **array** has a rank of one, the result is a scalar.

If **dim** is present, the result is an array with a rank one less than the
rank of **array**, and a size corresponding to the size of **array** with the
**dim** dimension removed. In all cases, the result is of the same type and
kind as **array**.

### **Examples**

sample program:
```fortran
program demo_minval
implicit none
integer :: i
character(len=*),parameter :: g='(3x,*(g0,1x))'

integer,save :: ints(3,5)= reshape([&
       1,  -2,   3,   4,   5,  &
      10,  20, -30,  40,  50,  &
      11,  22,  33, -44,  55  &
],shape(ints),order=[2,1])

integer,save :: box(3,5,2)

   box(:,:,1)=ints
   box(:,:,2)=-ints

   write(*,*)'Given the array'
   write(*,'(1x,*(g4.4,1x))') &
   & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))

   write(*,*)'What is the smallest element in the array?'
   write(*,g) minval(ints),'at <',minloc(ints),'>'

   write(*,*)'What is the smallest element in each column?'
   write(*,g) minval(ints,dim=1)

   write(*,*)'What is the smallest element in each row?'
   write(*,g) minval(ints,dim=2)

   ! notice the shape of the output has less columns
   ! than the input in this case
   write(*,*)'What is the smallest element in each column,'
   write(*,*)'considering only those elements that are'
   write(*,*)'greater than zero?'
   write(*,g) minval(ints, dim=1, mask = ints > 0)

   write(*,*)&
   & 'if everything is false a zero-sized array is NOT returned'
   write(*,*) minval(ints, dim=1, mask = .false.)
   write(*,*)'even for a zero-sized input'
   write(*,g) minval([integer ::], dim=1, mask = .false.)

   write(*,*)'a scalar answer for everything false is huge()'
   write(*,g) minval(ints, mask = .false.)
   write(*,g) minval([integer ::], mask = .false.)

   write(*,*)'some calls with three dimensions'
   write(*,g) minval(box, mask = .true. )
   write(*,g) minval(box, dim=1, mask = .true. )

   write(*,g) minval(box, dim=2, mask = .true. )
   write(*,g) 'shape of answer is ', &
   & shape(minval(box, dim=2, mask = .true. ))

end program demo_minval
```
Results:
```text
 Given the array
    1   -2    3    4    5
   10   20  -30   40   50
   11   22   33  -44   55

 What is the smallest element in the array?
   -44 at < 3 4 >
 What is the smallest element in each column?
   1 -2 -30 -44 5
 What is the smallest element in each row?
   -2 -30 -44
 What is the smallest element in each column,
 considering only those elements that are
 greater than zero?
   1 20 3 4 5
 if everything is false a zero-sized array is NOT returned
  2147483647  2147483647  2147483647  2147483647  2147483647
 even for a zero-sized input
   2147483647
 a scalar answer for everything false is huge()
   2147483647
   2147483647
 some calls with three dimensions
   -55
   1 -2 -30 -44 5 -11 -22 -33 -40 -55
   -2 -30 -44 -5 -50 -55
   shape of answer is  3 2
```
### **Standard**

Fortran 95

### **See Also**

[**min**(3)](#min),
[**minloc**(3)](#minloc)
[**maxloc**(3)](#maxloc),
[**maxval**(3)](#maxval),
[**min**(3)](#min)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## mod

### **Name**

**mod**(3) - \[NUMERIC\] Remainder function

### **Synopsis**
```fortran
    result = mod(a, p)
```
```fortran
     elemental type(TYPE(kind=KIND)) function mod(a,p)

      type(TYPE(kind=KIND),intent(in) :: a
      type(TYPE(kind=KIND),intent(in) :: p
```
### **Characteristics**

  The result and arguments are all of the same type and kind.
  The type may be any kind of _real_ or _integer_.

### **Description**

**mod**(3) computes the remainder of the division of **a** by **p**.

### **Options**

- **a**
  : Shall be a scalar of type _integer_ or _real_.

- **p**
  : Shall be a scalar of the same type and kind as **a** and not equal to
  zero.

### **Result**

The return value is the result of **a - (int(a/p) \* p)**. The type and kind
of the return value is the same as that of the arguments. The returned
value has the same sign as **a** and a magnitude less than the magnitude of
**p**.

### **Examples**

Sample program:

```fortran
program demo_mod
implicit none
     print *, mod(17,3)           ! yields 2
     print *, mod(17.5,5.5)       ! yields 1.0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0
     print *, mod(17.5d0,5.5d0)   ! yields 1.0d0

     print *, mod(-17,3)          ! yields -2
     print *, mod(-17.5,5.5)      ! yields -1.0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0
     print *, mod(-17.5d0,5.5d0)  ! yields -1.0d0

     print *, mod(17,-3)          ! yields 2
     print *, mod(17.5,-5.5)      ! yields 1.0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
     print *, mod(17.5d0,-5.5d0)  ! yields 1.0d0
end program demo_mod
```

Results:

```text
              2
      1.00000000
      1.0000000000000000
      1.0000000000000000
             -2
     -1.00000000
     -1.0000000000000000
     -1.0000000000000000
              2
      1.00000000
      1.0000000000000000
      1.0000000000000000
```

### **Standard**

FORTRAN 77

### **See Also**

[**modulo**(3)](#modulo)

 _fortran-lang intrinsic descriptions_

## modulo

### **Name**

**modulo**(3) - \[NUMERIC\] Modulo function

### **Synopsis**
```fortran
    result = modulo(a, p)
```
```fortran
     elemental TYPE(kind=KIND) function modulo(a,p)

      TYPE(kind=KIND),intent(in) :: a
      TYPE(kind=KIND),intent(in) :: p
```
### **Characteristics**

  The result and arguments are all of the same type and kind.
  The type may be any kind of _real_ or _integer_.

### **Description**

**modulo**(3) computes the **a** modulo **p**.

### **Options**

- **a**
  : Shall be a scalar of type _integer_ or _real_.

- **p**
  : Shall be a scalar of the same type and kind as **a**. It shall not be
  zero.

### **Result**

The type and kind of the result are those of the arguments.

- If **a** and **p** are of type _integer_: **modulo(a,p)** has the value of
  **a - floor (real(a) / real(p)) \* p**.

- If **a** and **p** are of type _real_: **modulo(a,p)** has the value of
  **a - floor (a / p) \* p**.

The returned value has the same sign as **p** and a magnitude less than the
magnitude of **p**.

### **Examples**

Sample program:

```fortran
program demo_modulo
implicit none
     print *, modulo(17,3)        ! yields 2
     print *, modulo(17.5,5.5)    ! yields 1.0

     print *, modulo(-17,3)       ! yields 1
     print *, modulo(-17.5,5.5)   ! yields 4.5

     print *, modulo(17,-3)       ! yields -1
     print *, modulo(17.5,-5.5)   ! yields -4.5
end program demo_modulo
```

Results:

```text
              2
      1.00000000
              1
      4.50000000
             -1
     -4.50000000
```

### **Standard**

Fortran 95

### **See Also**

[**mod**(3)](#mod)

 _fortran-lang intrinsic descriptions_

## move_alloc

### **Name**

**move_alloc**(3) - \[\] Move allocation from one object to another

### **Synopsis**
```fortran
    call move_alloc(from, to [,stat] [,errmsg] )
```
```fortran
     subroutine move_alloc(from, to)

      type(TYPE(kind=**)),intent(inout),allocatable :: from(..)
      type(TYPE(kind=**)),intent(out),allocatable   :: to(..)
      integer(kind=**),intent(out)     :: stat
      character(len=*),intent(inout)   :: errmsg
```
### **Characteristics**

- **from** may be of any type and kind.
- **to** shall be of the same type, kind and rank as **from**.

### **Description**

**move_alloc**(3) moves the allocation from **from** to
**to**. **from** will become deallocated in the process.

This is potentially more efficient than other methods of assigning
the values in **from** to **to** and explicitly deallocating **from**,
which are for more likely to require a temporary object or a copy of
the elements of the array.

### **Options**

- **from**
  : The data object to be moved to **to** and deallocated.

- **to**
  : The destination data object to move the allocated data object **from**
  to. Typically, it is a different shape than **from**.

- **stat**
  : If STAT is present and execution is successful, it is assigned the
    value zero.
  : If an error condition occurs,
      o if STAT is absent, error termination is initiated;
      o otherwise, if FROM is a coarray and the current team contains a
        stopped image, STAT is assigned the value STAT_STOPPED_IMAGE
        from the intrinsic module ISO_FORTRAN_ENV;
      o otherwise, if FROM is a coarray and the current team contains
      a failed image, and no other error condition
        occurs, STAT is assigned the value STAT_FAILED_IMAGE from the
        intrinsic module ISO_FORTRAN_ENV;
      o otherwise, STAT is assigned a processor-dependent positive value
        that differs from that of STAT_STOPPED_IMAGE or STAT_FAILED_IMAGE.

- **errmsg**

### **Examples**

Basic sample program to allocate a bigger grid

```fortran
program demo_move_alloc
implicit none
! Example to allocate a bigger GRID
real, allocatable :: grid(:), tempgrid(:)
integer :: n, i

   ! initialize small GRID
   n = 3
   allocate (grid(1:n))
   grid = [ (real (i), i=1,n) ]

   ! initialize TEMPGRID which will be used to replace GRID
   allocate (tempgrid(1:2*n))    ! Allocate bigger grid
   tempgrid(::2)  = grid         ! Distribute values to new locations
   tempgrid(2::2) = grid + 0.5   ! initialize other values

   ! move TEMPGRID to GRID
   call MOVE_ALLOC (from=tempgrid, to=grid)

   ! TEMPGRID should no longer be allocated
   ! and GRID should be the size TEMPGRID was
   if (size (grid) /= 2*n .or. allocated (tempgrid)) then
      print *, "Failure in move_alloc!"
   endif
   print *, allocated(grid), allocated(tempgrid)
   print '(99f8.3)', grid
end program demo_move_alloc
```

Results:

```text
    T F
      1.000   1.500   2.000   2.500   3.000   3.500
```

### **Standard**

Fortran 2003

### **See Also**

[**allocated**(3)](#allocated)

 _fortran-lang intrinsic descriptions_
<!--
    35  2 Class. Subroutine, pure if and only if FROM is not a coarray.
    1   3 Arguments.
    2     FROM      may be of any type, rank, and corank. It shall be allocatable and shall not be a coindexed object.
    3               It is an INTENT (INOUT) argument.
    4     TO        shall be type compatible (7.3.2.3) with FROM and have the same rank and corank. It shall be
    5               allocatable and shall not be a coindexed object. It shall be polymorphic if FROM is polymorphic.
    6               It is an INTENT (OUT) argument. Each nondeferred parameter of the declared type of TO shall
    7               have the same value as the corresponding parameter of the declared type of FROM.
    8     STAT (optional) shall be a noncoindexed integer scalar with a decimal exponent range of at least four. It is an
    9               INTENT (OUT) argument.
    10    ERRMSG(optional) shall be a noncoindexed default character scalar. It is an INTENT (INOUT) argument.

    11  4 If execution of MOVE_ALLOC is successful, or if STAT_FAILED_IMAGE is assigned to STAT,
    12          On invocation of MOVE_ALLOC, if the allocation status of TO is allocated, it is deallocated. Then,
    13        if FROM has an allocation status of allocated on entry to MOVE_ALLOC, TO becomes allocated with
    14        dynamic type, type parameters, bounds, cobounds, and value identical to those that FROM had on entry
    15        to MOVE_ALLOC. Note that if FROM and TO are the same variable, it shall be unallocated when
    16        MOVE_ALLOCisinvoked.
    17          If TO has the TARGETattribute, any pointer associated with FROM on entry to MOVE_ALLOC becomes
    18        correspondingly associated with TO. If TO does not have the TARGET attribute, the pointer association
    19        status of any pointer associated with FROM on entry becomes undefined.
    20          The allocation status of FROM becomes unallocated.
    21  5 Whenareference to MOVE_ALLOCisexecuted for which the FROM argument is a coarray, there is an implicit
    22    synchronization of all active images of the current team. On those images, execution of the segment (11.6.2)
    23    following the CALL statement is delayed until all other active images of the current team have executed the same
    24    statement the same number of times. When such a reference is executed, if any image of the current team has
    25    stopped or failed, an error condition occurs.
    36  8 If the ERRMSG argument is present and an error condition occurs, it is assigned an explanatory message. If no
    37    error condition occurs, the definition status and value of ERRMSG are unchanged.
    38  9 Example. The example below demonstrates reallocation of GRID to twice its previous size, with its previous
    39    contents evenly distributed over the new elements so that intermediate points can be inserted.
    40          REAL,ALLOCATABLE :: GRID(:),TEMPGRID(:)
    41          . . .
    42          ALLOCATE(GRID(-N:N))   ! initial allocation of GRID
    43          . . .
    44          ALLOCATE(TEMPGRID(-2*N:2*N)) ! allocate bigger grid
    45          TEMPGRID(::2)=GRID ! distribute values to new locations
    46          CALL MOVE_ALLOC(TO=GRID,FROM=TEMPGRID)
     1      The old grid is deallocated because TO is INTENT (OUT), and GRID then takes over the new grid allocation.
                NOTE1
                It is expected that the implementation of allocatable objects will typically involve descriptors to locate the
                allocated storage; MOVE_ALLOCcouldthenbeimplementedbytransferringthecontentsofthedescriptor
                for FROM to the descriptor for TO and clearing the descriptor for FROM.
-->

## mvbits

### **Name**

**mvbits**(3) - \[BIT:COPY\] Reproduce bit patterns found in one integer in another

### **Synopsis**
```fortran
   call mvbits(from, frompos, len, to, topos)
```
```fortran
    elemental subroutine mvbits(from,frompos,len,to,topos)

     integer(kind=KIND),intent(in)    :: from
     integer(kind=KIND),intent(in)    :: frompos
     integer(kind=KIND),intent(in)    :: len
     integer(kind=KIND),intent(inout) :: to
     integer(kind=KIND),intent(in)    :: topos
```
### **Characteristics**

### **Description**

**mvbits**(3) copies a bit pattern found in a range of adjacent bits in
the _integer_ **from** to a specified position in another integer **to**
(which is of the same kind as **from**). It otherwise leaves the bits
in **to** as-is.

The bit positions copied must exist within the value of **from**.
That is, the values of **frompos+len-1** and **topos+len-1** must be
nonnegative and less than **bit_size**(from).

The bits are numbered **0** to **bit_size(i)-1**, from right to left.

### **Options**

- **from**
  : An _integer_ to read bits from.

- **frompos**
  : **frompos** is the position of the first bit to copy. It is a
  nonnegative _integer_ value < **bit_size(from)**.

- **len**
  : A nonnegative _integer_ value that indicates how many bits to
  copy from **from**. It must not specify copying bits past the end
  of **from**. That is, **frompos + len** must be less than or equal
  to **bit_size(from)**.

- **to**
  : The _integer_ variable to place the copied bits into. It must
  be of the same kind as **from** and may even be the same variable
  as **from**.

  **to** is set by copying the sequence of bits of length **len**,
  starting at position **frompos** of **from** to position **topos** of
  **to**. No other bits of **to** are altered. On return, the **len**
  bits of **to** starting at **topos** are equal to the value that
  the **len** bits of **from** starting at **frompos** had on entry.

- **topos**
  : A nonnegative _integer_ value indicating the starting location in
  **to** to place the specified copy of bits from **from**.
  **topos + len** must be less than or equal to **bit_size(to)**.

### **Examples**

Sample program that populates a new 32-bit integer with its bytes in
reverse order (ie. changes the Endian of the integer).
```fortran
program demo_mvbits
use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
implicit none
integer(kind=int32) :: intfrom, intto, abcd_int
character(len=*),parameter :: bits= '(g0,t30,b32.32)'
character(len=*),parameter :: fmt= '(g0,t30,a,t40,b32.32)'

    intfrom=huge(0)  ! all bits are 1 accept the sign bit
    intto=0          ! all bits are 0

    !! CHANGE BIT 0
    ! show the value and bit pattern
    write(*,bits)intfrom,intfrom
    write(*,bits)intto,intto

    ! copy bit 0 from intfrom to intto to show the rightmost bit changes
    !          (from,    frompos, len,    to, topos)
    call mvbits(intfrom,       0,   1, intto,     0) ! change bit 0
    write(*,bits)intto,intto

    !! COPY PART OF A VALUE TO ITSELF
    ! can copy bit from a value to itself
    call mvbits(intfrom,0,1,intfrom,31)
    write(*,bits)intfrom,intfrom

    !! MOVING BYTES AT A TIME
    ! make native integer value with bit patterns
    ! that happen to be the same as the beginning of the alphabet
    ! to make it easy to see the bytes are reversed
    abcd_int=transfer('abcd',0)
    ! show the value and bit pattern
    write(*,*)'native'
    write(*,fmt)abcd_int,abcd_int,abcd_int

    ! change endian of the value
    abcd_int=int_swap32(abcd_int)
    ! show the values and their bit pattern
    write(*,*)'non-native'
    write(*,fmt)abcd_int,abcd_int,abcd_int

 contains

 pure elemental function int_swap32(intin) result(intout)
 ! Convert a 32 bit integer from big Endian to little Endian,
 ! or conversely from little Endian to big Endian.
 !
 integer(kind=int32), intent(in)  :: intin
 integer(kind=int32) :: intout
    ! copy bytes from input value to new position in output value
    !          (from,  frompos, len,     to, topos)
    call mvbits(intin,       0,   8, intout,    24) ! byte1 to byte4
    call mvbits(intin,       8,   8, intout,    16) ! byte2 to byte3
    call mvbits(intin,      16,   8, intout,     8) ! byte3 to byte2
    call mvbits(intin,      24,   8, intout,     0) ! byte4 to byte1
 end function int_swap32

 end program demo_mvbits
````
Results:
```text

   2147483647                   01111111111111111111111111111111
   0                            00000000000000000000000000000000
   1                            00000000000000000000000000000001
   -1                           11111111111111111111111111111111
    native
   1684234849                   abcd      01100100011000110110001001100001
    non-native
   1633837924                   dcba      01100001011000100110001101100100
````

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## nearest

### **Name**

**nearest**(3) - \[MODEL_COMPONENTS\] Nearest representable number

### **Synopsis**
```fortran
    result = nearest(x, s)
```
```fortran
     elemental real(kind=KIND) function nearest(x,s)

      real(kind=KIND),intent(in) :: x
      real(kind=**),intent(in) :: s
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **x** may be a _real_ value of any kind.
- The return value is of the same type and kind as **x**.

### **Description**

**nearest**(3) returns the processor-representable number nearest to
**x** in the direction indicated by the sign of **s**.

### **Options**

- **x**
  : the value to find the nearest representable value of

- **s**
  : a non-zero value whose sign is used to determine the direction in
  which to search from **xx** to the representable value.

  If **s** is positive, **nearest** returns the processor-representable
  number greater than **x** and nearest to it.

  If **s** is negative, **nearest** returns the processor-representable
  number smaller than **x** and nearest to it.

### **Result**

The return value is of the same type as **x**. If **s** is positive, **nearest**
returns the processor-representable number greater than **x** and nearest to
it. If **s** is negative, **nearest** returns the processor-representable number
smaller than **x** and nearest to it.

### **Examples**

Sample program:

```fortran
program demo_nearest
implicit none

   real :: x, y
   x = nearest(42.0, 1.0)
   y = nearest(42.0, -1.0)
   write (*,"(3(g20.15))") x, y, x - y

!  write (*,"(3(g20.15))") &
!   nearest(tiny(0.0),1.0), &
!   nearest(tiny(0.0),-1.0), &
!   nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)

!  write (*,"(3(g20.15))") &
!   nearest(huge(0.0),1.0), &
!   nearest(huge(0.0),-1.0), &
!   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)

end program demo_nearest
```
Results:
```text
   42.0000038146973    41.9999961853027    .762939453125000E-05
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## new_line

### **Name**

**new_line**(3) - \[CHARACTER\] New-line character

### **Synopsis**
```fortran
    result = new_line(c)
```
```fortran
     character(len=1,kind=kind(c)) function new_line(c)

      character(len=1,kind=KIND),intent(in) :: c(..)
```
### **Characteristics**

### **Description**

**new_line**(3) returns the new-line character.

Case (i)
: If **a** is default _character_ and the character in position **10** of the
ASCII collating sequence is representable in the default character set,
then the result is **achar(10)**.

Case (ii)
: If **a** is an ASCII character or an ISO 10646 character, then the
result is **char(10, kind (a))**.

Case (iii)
: Otherwise, the result is a processor-dependent character that
represents a newline in output to files connected for formatted
stream output if there is such a character.

Case (iv)
: Otherwise, the result is the blank character.

### **Options**

- **C**
  : The argument shall be a scalar or array of the type _character_.

### **Result**

Returns a _character_ scalar of length one with the new-line character of
the same kind as parameter **c**.

### **Examples**

Sample program:
```fortran
program demo_new_line
implicit none
character,parameter :: nl=new_line('a')
character(len=:),allocatable :: string

   string='This is record 1.'//nl//'This is record 2.'
   write(*,'(a)') string

   write(*,'(*(a))',advance='no') &
      nl,'This is record 1.',nl,'This is record 2.',nl

end program demo_new_line
```
Results:

```text
   This is record 1.
   This is record 2.

   This is record 1.
   This is record 2.
```
### **Standard**

Fortran 2003

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## nint

### **Name**

**nint**(3) - \[TYPE:NUMERIC\] Nearest whole number

### **Synopsis**
```fortran
    result = nint( x [,kind] )
```
```fortran
     elemental integer(kind=KIND) function nint(x, kind )

      real(kind=**),intent(in) :: x
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

  - The result is default _integer_ kind or the value of **kind**
    if it is present.

### **Description**

**nint**(3) rounds its argument to the nearest whole number with its
sign preserved.

The user must ensure the value is a valid value for the range of the
**kind** returned. If the processor cannot represent the result in the kind
specified, the result is undefined.

If **x** is greater than zero, **nint(x)** has the value **int(x+0.5)**.

If **x** is less than or equal to zero, **nint(x)** has the value
**int(a-0.5)**.

### **Options**

- **x**
  : The type of the argument shall be _real_.

- **kind**
  : (Optional) A constant _integer_ expression indicating the kind
  parameter of the result. Otherwise, the kind type parameter is that
  of default _integer_ type.

### **Result**

- **answer**
  : The result is the integer nearest **x**, or if there are two integers
  equally near **x**, the result is whichever such _integer_ has the greater
  magnitude.

  The result is undefined if it cannot be represented in the specified
  integer type.

### **Examples**

Sample program:
```fortran
program demo_nint
implicit none
integer,parameter :: dp=kind(0.0d0)
real              :: x4 = 1.234E0
real(kind=dp)     :: x8 = 4.721_dp

! basic use
   print *, nint(x4), nint(x8),nint(-x8)
   ! elemental
   print *,nint([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

! issues
ISSUES: block
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
integer :: icheck
   ! make sure input is in range for the type returned
   write(*,*)'Range limits for typical KINDS:'
   write(*,'(1x,g0,1x,g0)')  &
   & int8,huge(0_int8),   &
   & int16,huge(0_int16), &
   & int32,huge(0_int32), &
   & int64,huge(0_int64)

   ! the standard does not require this to be an error ...
   x8=12345.67e15 ! too big of a number
   icheck=selected_int_kind(ceiling(log10(x8)))
   write(*,*)'Any KIND big enough? ICHECK=',icheck
   print *, 'These are all wrong answers for ',x8
   print *, nint(x8,kind=int8)
   print *, nint(x8,kind=int16)
   print *, nint(x8,kind=int32)
   print *, nint(x8,kind=int64)
endblock ISSUES

end program demo_nint
```
Results:
```text
   >  1    5   -5
   > -3   -3   -2   -2   -2
   > -1   -1    0    1    1
   >  2    2    2    3    3
   > Range limits for typical KINDS:
   > 1 127
   > 2 32767
   > 4 2147483647
   > 8 9223372036854775807
   > Any KIND big enough? ICHECK=          16
   > These are all wrong answers for    1.2345669499901444E+019
   >    0
   >      0
   >           0
   > -9223372036854775808
```
### **Standard**

FORTRAN 77 , with KIND argument - Fortran 90

### **See Also**

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**int**(3)](#int),
[**selected_int_kind**(3)](#selected_int_kind),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## norm2

### **Name**

**norm2**(3) - \[MATHEMATICS\] Euclidean vector norm

### **Synopsis**
```fortran
    result = norm2(array, [dim])
```
```fortran
     real function norm2(array, dim)

      real,intent(in) :: array(..)
      integer,intent(in),optional :: dim
```
### **Characteristics**

- **array** shall be an array of type _real_.
- **dim** shall be a scalar of type _integer_
- The result is of the same type as **array**.

### **Description**

  **norm2**(3) calculates the Euclidean vector norm (L_2 norm) of
  **array** along dimension **dim**.

### **Options**

- **array**
  : the array of input values for the L_2 norm computations

- **dim**
  : a value in the range from **1** to **rank(array)**.

### **Result**

  If **dim** is absent, a scalar with the square root of the sum of squares
  of the elements of **array** is returned.

  Otherwise, an array of rank **n-1**, where **n** equals the rank of
  **array**, and a shape similar to that of **array** with dimension DIM
  dropped is returned.

### **Examples**

Sample program:

```fortran
program demo_norm2
implicit none

real :: x(3,3) = reshape([ &
   1, 2, 3, &
   4, 5, 6, &
   7, 8, 9  &
   ],shape(x),order=[2,1])

  write(*,*) 'x='
  write(*,'(4x,3f4.0)')transpose(x)

  write(*,*) 'norm2(x)=',norm2(x)

  write(*,*) 'x**2='
  write(*,'(4x,3f4.0)')transpose(x**2)
  write(*,*)'sqrt(sum(x**2))=',sqrt(sum(x**2))

end program demo_norm2
```
Results:
```text
 x=
      1.  2.  3.
      4.  5.  6.
      7.  8.  9.
 norm2(x)=   16.88194
 x**2=
      1.  4.  9.
     16. 25. 36.
     49. 64. 81.
 sqrt(sum(x**2))=   16.88194
```
### **Standard**

Fortran 2008

### **See Also**

[**product**(3)](#product),
[**sum**(3)](#sum),
[**hypot**(3)](#hypot)

 _fortran-lang intrinsic descriptions_

## not

### **Name**

**not**(3) - \[BIT:LOGICAL\] Logical negation

### **Synopsis**
```fortran
    result = not(i)
```
```fortran
    elemental integer(kind=KIND) function not(i)

     integer(kind=KIND), intent(in) :: i
```
### **Characteristics**

The return type is of the same kind as the argument.

### **Description**

**not**(3) returns the bitwise Boolean inverse of **i**.

### **Options**

- **i**
  : The type shall be _integer_.

### **Result**

The return type is _integer_, of the same kind as the argument.

### **Examples**

Sample program

```fortran
program demo_not
implicit none
integer :: i

   i=13741
   write(*,'(b32.32,1x,i0)')i,i
   write(*,'(b32.32,1x,i0)')not(i),not(i)

end program demo_not
```

Results:

```
   00000000000000000011010110101101 13741
   11111111111111111100101001010010 -13742
```

### **Standard**

Fortran 95

### **See Also**

[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),

[**ibclr**(3)](#ibclr)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## null

### **Name**

**null**(3) - \[TRANSFORMATIONAL\] Function that returns a disassociated pointer

### **Synopsis**
```fortran
    ptr => null( [mold] )
```
```fortran
     function null(mold)

      type(TYPE(kind=**)),pointer,optional :: mold
```
### **Characteristics**

- **mold** is a pointer of any association status and of any type.
- The result is a disassociated pointer or an unallocated allocatable entity.

### **Description**

  **null**(3) returns a disassociated pointer.

  If **mold** is present, a disassociated pointer of the same type is
  returned, otherwise the type is determined by context.

  In _Fortran 95_, **mold** is optional. Please note that _Fortran 2003_
  includes cases where it is required.

### **Options**

- **mold**
  : a pointer of any association status and of any
  type.

### **Result**

   A disassociated pointer or an unallocated allocatable entity.

### **Examples**

Sample program:

```fortran
!program demo_null
module showit
implicit none
private
character(len=*),parameter :: g='(*(g0,1x))'
public gen
! a generic interface that only differs in the
! type of the pointer the second argument is
interface gen
 module procedure s1
 module procedure s2
end interface

contains

subroutine s1 (j, pi)
 integer j
 integer, pointer :: pi
   if(associated(pi))then
      write(*,g)'Two integers in S1:,',j,'and',pi
   else
      write(*,g)'One integer in S1:,',j
   endif
end subroutine s1

subroutine s2 (k, pr)
 integer k
 real, pointer :: pr
   if(associated(pr))then
      write(*,g)'integer and real in S2:,',k,'and',pr
   else
      write(*,g)'One integer in S2:,',k
   endif
end subroutine s2

end module showit

use showit, only : gen

real,target :: x = 200.0
integer,target :: i = 100

real, pointer :: real_ptr
integer, pointer :: integer_ptr

! so how do we call S1() or S2() with a disassociated pointer?

! the answer is the null() function with a mold value

! since s1() and s2() both have a first integer
! argument the NULL() pointer must be associated
! to a real or integer type via the mold option
! so the following can distinguish whether s1(1)
! or s2() is called, even though the pointers are
! not associated or defined

call gen (1, null (real_ptr) )    ! invokes s2
call gen (2, null (integer_ptr) ) ! invokes s1
real_ptr => x
integer_ptr => i
call gen (3, real_ptr ) ! invokes s2
call gen (4, integer_ptr ) ! invokes s1

end
!end program demo_null
```
Results:
```text
   One integer in S2:, 1
   One integer in S1:, 2
   integer and real in S2:, 3 and 200.000000
   Two integers in S1:, 4 and 100
```
### **Standard**

Fortran 95

### **See Also**

[**associated**(3)](#associated)

 _fortran-lang intrinsic descriptions_

## num_images

### **Name**

**num_images**(3) - \[COLLECTIVE\] Number of images

### **Synopsis**
```fortran
    result = num_images()
```
```fortran
     integer function num_images ()
```
or
```fortran
    result = num_images(team)
```
```fortran
     integer function num_images (team)

      type(TEAM_TYPE),intent(in) :: team
```
or
```fortran
    result = num_images(team_number)
```
```fortran
     integer function num_images (team_number)

      integer(kind=KIND),intent(in) :: team_number
```
### **Characteristics**

### **Description**

**num_images**(3) Returns the number of images.

### **Options**

- **team**
  : shall  be a scalar of type TEAM_TYPE from the intrinsic module
  ISO_FORTRAN_ENV, with a value that identifies the current or an
  ancestor team.

- **team_number**
  : shall be an integer scalar. It shall identify the initial team or
  a team whose parent is the same as that of the current team.

### **Result**

  The number of images in the specified team, or in the current team if
  no team is specified.

### **Examples**

Sample program:

```fortran
program demo_num_images
implicit none
integer :: value[*]
integer :: i

   value = this_image()
   sync all
   if (this_image() == 1) then
     do i = 1, num_images()
       write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
     end do
   endif

end program demo_num_images
```

### **Standard**

Fortran 2008 . With DISTANCE or FAILED argument, TS 18508

### **See Also**

[**this_image**(3)](#this_image),
[**image_index**(3)](#this_index)

 _fortran-lang intrinsic descriptions_

## out_of_range

### **Name**

**out_of_range**(3) - \[TYPE:NUMERIC\] Whether a value cannot be converted safely.

### **Synopsis**
```fortran
    result = out_of_range (x, mold [, round])
```
```fortran
     elemental logical function(x, mold, round)

      TYPE,kind=KIND),intent(in) :: x
      TYPE,kind=KIND),intent(in) :: mold
      logical,intent(in),optional     :: round
```
### **Characteristics**

   where TYPE may be _real_ or _integer_ of any available KIND.

### **Description**

   **out_of_range**(3) determines whether a value **x** can be converted
   safely to a _real_ or _integer_ variable the same type and kind as
   **mold**.

### **Options**
   - **x**
     : a scalar of type _integer_ or _real_ to be tested for whether
     it can be stored in a variable of the type and kind of **mold**

   - **mold**
     : shall be an _integer_ or _real_ scalar. If it is a variable, it
     need not be defined, as only the type and kind are queried.

   - **round**
     : flag whether to round the value of **xx** before validating it as
     an integer value like **mold**.

     **round** can only be present if **x** is of type
     _real_ and **mold** is of type _integer_.

### **Result**

From the standard:

   Case (i):     If MOLD is of type integer, and ROUND is absent or
                 present with the value false, the result is true
                 if and only if the value of X is an IEEE infinity or
                 NaN, or if the integer with largest magnitude that lies
                 between zero and X inclusive is not representable by
                 objects with the type and kind of MOLD.

   Case (ii):    If MOLD is of type integer, and ROUND is present with
                 the value true, the result is true if and only
                 if the value of X is an IEEE infinity or NaN, or
                 if the integer nearest X, or the integer of greater
                 magnitude if two integers are equally near to X, is not
                 representable by objects with the type and kind of MOLD.

   Case (iii):   Otherwise, the result is true if and only if the value
                 of X is an IEEE infinity or NaN that is not
                 supported by objects of the type and kind of MOLD,
                 or if X is a finite number and the result of rounding
                 the value of X (according to the IEEE rounding mode if
                 appropriate) to the extended model for the kind of MOLD
                 has magnitude larger than that of the largest finite
                 number with the same sign as X that is representable
                 by objects with the type and kind of MOLD.

   NOTE

   MOLD is required to be a scalar because the only information
   taken from it is its type and kind. Allowing an array MOLD would
   require that it be conformable with X. ROUND is scalar because
   allowing an array rounding mode would have severe performance
   difficulties on many processors.

### **Examples**

Sample program:

```fortran
program demo_out_of_range
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
integer            :: i
integer(kind=int8) :: i8, j8

    ! compilers are not required to produce an error on out of range.
    ! here storing the default integers into 1-byte integers
    ! incorrectly can have unexpected results
    do i=127,130
       i8=i
       j8=-i
       ! OUT_OF_RANGE(3f) can let you check if the value will fit
       write(*,*)i8,j8,' might have expected',i,-i, &
        & out_of_range( i,i8), &
        & out_of_range(-i,i8)
    enddo
    write(*,*) 'RANGE IS ',-1-huge(0_int8),'TO',huge(0_int8)
    ! the real -128.5 is truncated to -128 and is in range
    write(*,*) out_of_range (  -128.5, 0_int8)         ! false

    ! the real -128.5 is rounded to -129 and is not in range
    write(*,*) out_of_range (  -128.5, 0_int8, .true.) ! true

end program demo_out_of_range
```

Results:

```text
  >  127 -127  might have expected         127        -127 F F
  > -128 -128  might have expected         128        -128 T F
  > -127  127  might have expected         129        -129 T T
  > -126  126  might have expected         130        -130 T T
  > RANGE IS         -128 TO  127
  > F
  > T
```

### **Standard**

   FORTRAN 2018

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## pack

### **Name**

**pack**(3) - \[ARRAY CONSTRUCTION\] Pack an array into an array of rank one

### **Synopsis**
```fortran
    result = pack( array, mask [,vector] )
```
```fortran
     TYPE(kind=KIND) function pack(array,mask,vector)

      TYPE(kind=KIND),option(in) :: array(..)
      logical  :: mask(*)
      TYPE(kind=KIND),option(in),optional :: vector(*)
```
### **Characteristics**

  - **array**, **vector** and the returned value are all of the same kind and type.
  - **mask** may be a scalar as well an an array.

### **Description**

  **pack**(3) stores the elements of ARRAY in an array of rank one.

  The beginning of the resulting array is made up of elements whose
  **mask** equals _.true._. Afterwards, positions are filled with elements
  taken from **vector**.

### **Options**

- **array**
  : The data from this array is used to fill the resulting vector

- **mask**
  : the _logical_ mask must be the same size as **array** or,
  alternatively, it may be a _logical_ scalar.

- **vector**
  : (Optional) shall be an array of the same type as **array** and of rank
  one. If present, the number of elements in **vector** shall be equal to
  or greater than the number of true elements in **mask**. If **mask** is
  scalar, the number of elements in **vector** shall be equal to or
  greater than the number of elements in **array**.

### **Result**

The result is an array of rank one and the same type as that of **array**.
If **vector** is present, the result size is that of **vector**, the number of
_.true._ values in **mask** otherwise.

### **Examples**

Sample program:

```fortran
program demo_pack
implicit none
integer, allocatable :: m(:)
character(len=10) :: c(4)

 ! gathering nonzero elements from an array:
   m = [ 1, 0, 0, 0, 5, 0 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)

 ! Gathering nonzero elements from an array and appending elements
 ! from VECTOR till the size of the mask array (or array size if the
 ! mask is scalar):
   m = [ 1, 0, 0, 2 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])

 ! select strings whose second character is "a"
   c = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
   write(*, fmt="(*(g0, ' '))") pack(c, c(:)(2:2) == 'a' )

end program demo_pack
```
Results:
```text
   1 5
   1 2 3 4
   bat        cat
```
### **Standard**

Fortran 95

### **See Also**

[**merge**(3)](#merge),
[**spread**(3)](#spread),
[**unpack**(3)](#unpack)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## parity

### **Name**

**parity**(3) - \[TRANSFORMATIONAL\] Reduction with exclusive **OR**()

### **Synopsis**
```fortran
    result = parity( mask [,dim] )
```
```fortran
     logical(kind=KIND) function parity(mask, dim)

      type(logical(kind=KIND)),intent(in)        :: mask(..)
      type(integer(kind=**)),intent(in),optional :: dim
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

### **Description**

**parity**(3) calculates the parity (i.e. the reduction using .xor.) of
__mask__ along dimension __dim__.

### **Options**

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __mask__.

### **Result**

The result is of the same type as __mask__.

If __dim__ is absent, a scalar with the parity of all elements in __mask__
is returned: __.true.__ if an odd number of elements are __.true.__
and __.false.__ otherwise.

When __dim__ is specified the returned shape is similar to that of
__mask__ with dimension __dim__ dropped.

### **Examples**

Sample program:
```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x)
end program demo_parity
````
Results:
```text
    T
### **Standard**

Fortran 2008

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## popcnt

### **Name**

**popcnt**(3) - \[BIT:COUNT\] Number of bits set

### **Synopsis**
```fortran
    result = popcnt(i)
```
```fortran
     elemental integer function popcnt(i)

      integer(kind=KIND), intent(in) :: i
```
### **Characteristics**

- **i** argument may be an integer of any kind.
- The return value is an _integer_ of the default integer kind.

### **Description**

**popcnt**(3) returns the number of bits set in the binary representation
of an _integer_.

### **Options**

- **i**
  : value to count set bits in

### **Result**

The number of bits set in **i**.

### **Examples**

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
character(len=*),parameter :: pretty='(b64,1x,i0)'
   ! basic usage
   print pretty, 127,     popcnt(127)
   print pretty, int(b"01010"), popcnt(int(b"01010"))
   ! any kind of an integer can be used
   print pretty, huge(0_int8),  popcnt(huge(0_int8))
   print pretty, huge(0_int16), popcnt(huge(0_int16))
   print pretty, huge(0_int32), popcnt(huge(0_int32))
   print pretty, huge(0_int64), popcnt(huge(0_int64))
end program demo_popcnt
```
Results:

Note that on most machines the first bit is the sign bit, and a zero is
used for positive values, but that this is system-dependent.  These are
typical values, where the huge(3f) function has set all but the first
bit to 1.
```text
 >                                                         1111111 7
 >                                                            1010 2
 >                                                         1111111 7
 >                                                 111111111111111 15
 >                                 1111111111111111111111111111111 31
 > 111111111111111111111111111111111111111111111111111111111111111 63
```
### **Standard**

Fortran 2008

### **See Also**

There are many procedures that operator or query values at the bit level:

[**poppar**(3)](#poppar),
[**leadz**(3)](#leadz),
[**trailz**(3)](#trailz)
[**atomic_and**(3)](#atomic_and),
[**atomic_fetch_and**(3)](#atomic_fetch_and),
[**atomic_fetch_or**(3)](#atomic_fetch_or),
[**atomic_fetch_xor**(3)](#atomic_fetch_xor),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor),
[**bge**(3)](#bge),
[**bge**(3)](#bge),
[**bgt**(3)](#bgt),
[**bit_size**(3)](#bit_size),
[**ble**(3)](#ble),
[**blt**(3)](#blt),
[**btest**(3)](#btest),
[**dshiftl**(3)](#dshiftl),
[**dshiftr**(3)](#dshiftr),
[**iall**(3)](#iall),
[**iand**(3)](#iand),
[**iany**(3)](#iany),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**ieor**(3)](#ieor),
[**ior**(3)](#ior),
[**iparity**(3)](#iparity),
[**ishftc**(3)](#ishftc),
[**ishft**(3)](#ishft),
[**maskl**(3)](#maskl),
[**maskr**(3)](#maskr),
[**merge_bits**(3)](#merge_bits),
[**mvbits**(3)](#mvbits),
[**mvbits**(3)](#mvbits),
[**not**(3)](#not),
[**shifta**(3)](#shifta),
[**shiftl**(3)](#shiftl),
[**shiftr**(3)](#shiftr),
[**storage_size**(3)](#storage_size)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## poppar

### **Name**

**poppar**(3) - \[BIT:COUNT\] Parity of the number of bits set

### **Synopsis**
```fortran
    result = poppar(i)
```
```fortran
     elemental integer function poppar(i)

      integer(kind=KIND), intent(in) :: i
```
### **Characteristics**

- **i** is an _integer_ of any kind
- the return value is a default kind _integer_

### **Description**

**poppar**(3) returns the parity of an integer's binary representation
(i.e., the parity of the number of bits set).

### **Options**

- **i**
  : The value to query the bits of

### **Result**

The return value is equal to **0** if **i** has an even number of bits
set and **1** if an odd number of bits are set.

### **Examples**

Sample program:

```fortran
program demo_poppar
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
character(len=*),parameter :: pretty='(b64,1x,i0)'
   ! basic usage
   print pretty, 127,     poppar(127)
   print pretty, 128,     poppar(128)
   print pretty, int(b"01010"), poppar(int(b"01010"))

   ! any kind of an integer can be used
   print pretty, huge(0_int8),  poppar(huge(0_int8))
   print pretty, huge(0_int16), poppar(huge(0_int16))
   print pretty, huge(0_int32), poppar(huge(0_int32))
   print pretty, huge(0_int64), poppar(huge(0_int64))
end program demo_poppar
```
Results:
```text
 >                                                          1111111 1
 >                                                         10000000 1
 >                                                             1010 0
 >                                  1111111111111111111111111111111 1
 >                                                          1111111 1
 >                                                  111111111111111 1
 >                                  1111111111111111111111111111111 1
 >  111111111111111111111111111111111111111111111111111111111111111 1
```
### **Standard**

Fortran 2008

### **See Also**

There are many procedures that operator or query values at the bit level:

[**popcnt**(3)](#popcnt),
[**leadz**(3)](#leadz),
[**trailz**(3)](#trailz)
[**atomic_and**(3)](#atomic_and),
[**atomic_fetch_and**(3)](#atomic_fetch_and),
[**atomic_fetch_or**(3)](#atomic_fetch_or),
[**atomic_fetch_xor**(3)](#atomic_fetch_xor),
[**atomic_or**(3)](#atomic_or),
[**atomic_xor**(3)](#atomic_xor),
[**bge**(3)](#bge),
[**bge**(3)](#bge),
[**bgt**(3)](#bgt),
[**bit_size**(3)](#bit_size),
[**ble**(3)](#ble),
[**blt**(3)](#blt),
[**btest**(3)](#btest),
[**dshiftl**(3)](#dshiftl),
[**dshiftr**(3)](#dshiftr),
[**iall**(3)](#iall),
[**iand**(3)](#iand),
[**iany**(3)](#iany),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**ieor**(3)](#ieor),
[**ior**(3)](#ior),
[**iparity**(3)](#iparity),
[**ishftc**(3)](#ishftc),
[**ishft**(3)](#ishft),
[**maskl**(3)](#maskl),
[**maskr**(3)](#maskr),
[**merge_bits**(3)](#merge_bits),
[**mvbits**(3)](#mvbits),
[**mvbits**(3)](#mvbits),
[**not**(3)](#not),
[**shifta**(3)](#shifta),
[**shiftl**(3)](#shiftl),
[**shiftr**(3)](#shiftr),
[**storage_size**(3)](#storage_size)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## precision

### **Name**

**precision**(3) - \[NUMERIC MODEL\] Decimal precision of a real kind

### **Synopsis**
```fortran
    result = precision(x)
```
```fortran
     integer function precision(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

where **TYPE** may be _real_ or _complex_

The return value is of type _integer_ and of the default integer kind.

### **Description**

**precision**(3) returns the decimal precision in the model of the type
of **x**.

### **Options**

- **x**
  : Shall be of type _real_ or _complex_.

### **Result**

   The precision of values of the type and kind of **x**

### **Examples**

Sample program:

```fortran
program demo_precision
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp)    :: x(2)
complex(kind=dp) :: y

   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_precision
```
Results:
```text
              6          37
             15         307
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## present

### **Name**

**present**(3) - [STATE\] Determine whether an optional dummy argument
is specified

### **Synopsis**
```fortran
    result = present(a)
```
```fortran
     logical function present (a)

      type(TYPE(kind=KIND)) :: a(..)
```
### **Characteristics**

- **a** May be of any type and may be a pointer, scalar or array value,
  or a dummy procedure.

### **Description**

  **present**(3) can be used in a procedure to determine if an optional
  dummy argument was present on the current call to the procedure.

  When an argument is not present when the current procedure is invoked,
  you may only pass it as an optional argument to another procedure or
  pass it as an argument to PRESENT.

### **Options**

- **a**
  : the name of an optional dummy argument accessible within the current
  subroutine or function.

### **Result**

  Returns _.true._ if the optional argument **a** is present (was passed
  on the call to the procedure) , or _.false._ otherwise.

### **Examples**

Sample program:
```fortran
program demo_present
implicit none
integer :: answer
   ! argument to func() is not present
   answer=func()
   write(*,*) answer
   ! argument to func() is present
   answer=func(1492)
   write(*,*) answer
contains

integer function func(x)
! the optional characteristic on this definition allows this variable
! to not be specified on a call; and also allows it to subsequently
! be passed to PRESENT(3f):
integer, intent(in), optional :: x
integer :: x_local

  ! basic
   if(present(x))then
     ! if present, you can use x like any other variable.
     x_local=x
   else
     ! if not, you cannot define or reference x except to
     ! pass it as an optional parameter to another procedure
     ! or in a call to present(3f)
     x_local=0
   endif

   func=x_local**2

  ! passing the argument on to other procedures
   ! so something like this is a bad idea because x is used
   ! as the first argument to merge(3f) when it might not be
   ! present
   ! xlocal=merge(x,0,present(x)) ! NO!!

   ! We can pass it to another procedure if and only if the
   ! other procedure declares the argument as optional as well.
   call tattle('optional argument x',x)
end function

subroutine tattle(label,arg)
character(len=*),intent(in) :: label
integer,intent(in),optional :: arg
   if(present(arg))then
      write(*,*)label,' is present'
   else
      write(*,*)label,' is not present'
   endif
end subroutine tattle

end program demo_present
```
Results:
```text
    optional argument x is not present
              0
    optional argument x is present
        2226064
```
### **Standard**

Fortran 95

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## product

### **Name**

**product**(3) - \[ARRAY REDUCTION\] Product of array elements

### **Synopsis**
```fortran
    result = product(array [,dim] [,mask])
```
```fortran
     NUMERIC function product(array, dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type
  - **NUMERIC** is any numeric type and kind.

### **Description**

**product**(3) multiplies together all the selected elements of **array**,
or along dimension **dim** if the corresponding element in **mask**
is _.true._.

If **dim** is absent, a scalar with the product of all elements in **array** is
returned. (Note a zero-sized **array** returns **1**).

When **dim** is present, If the masked array has a dimension of one
(ie. is a vector) the result is a scalar. Otherwise, an array of rank
**n-1**, where **n** equals the rank of **array**, and a shape similar
to that of **array** with dimension **dim** dropped is returned.

### **Options**

- **array**
  : Shall be an array of type _integer_, _real_ or _complex_.

- **dim**
  : shall be a scalar of type _integer_ with a value in the
  range from **1 to n**, where **n** equals the rank of **array**.

- **mask**
  : shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Result**

The result is of the same type as **array**.

### **Examples**

Sample program:
```fortran
program demo_product
implicit none
character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
character(len=1),parameter :: nl=new_line('a')

NO_DIM: block
!    If DIM is not specified, the result is the product of all the
!    selected array elements.
integer :: i,n, p1, p2
integer,allocatable :: array(:)
   ! all elements are selected by default
   do n=1,10
      print all, 'factorial of ',n,' is ', product([(real(i),i=1,n)])
   enddo

   ! using a mask
   array=[10,12,13,15,20,25,30]
   p1=product(array, mask=mod(array, 2)==1) ! only odd elements
   p2=product(array, mask=mod(array, 2)/=1) ! only even elements
   print all, nl,'product of all elements',product(array) ! all elements
   print all, ' odd * even =',nl,p1,'*',p2,'=',p1*p2

   ! NOTE: If ARRAY is a zero-sized array, the result is equal to one
   print all
   print all, 'zero-sized array=>',product([integer :: ])
   ! NOTE: If nothing in the mask is true, this also results in a null
   !       array
   print all, 'all elements have a false mask=>', &
            & product(array,mask=.false.)

endblock NO_DIM

WITH_DIM: block
integer :: rect(2,3)
integer :: box(2,3,4)

!  lets fill a few arrays
   rect = reshape([ &
     1, 2, 3,       &
     4, 5, 6        &
   ],shape(rect),order=[2,1])
   call print_matrix_int('rect',rect)

!  Find the product of each column in RECT.
   print all, 'product of columns=',product(rect, dim = 1)

! Find the product of each row in RECT.
   print all, 'product of rows=',product(rect, dim = 2)

! now lets try a box
   box(:,:,1)=rect
   box(:,:,2)=rect*(+10)
   box(:,:,3)=rect*(-10)
   box(:,:,4)=rect*2
   ! lets look at the values
   call print_matrix_int('box 1',box(:,:,1))
   call print_matrix_int('box 2',box(:,:,2))
   call print_matrix_int('box 3',box(:,:,3))
   call print_matrix_int('box 4',box(:,:,4))

   ! remember without dim= even a box produces a scalar
   print all, 'no dim gives a scalar',product(real(box))

   ! only one plane has negative values, so note all the "1" values
   ! for vectors with no elements
   call print_matrix_int('negative values', &
   & product(box,mask=box < 0,dim=1))

!   If DIM is specified and ARRAY has rank greater than one, the
!   result is a new array in which dimension DIM has been eliminated.

   ! pick a dimension to multiply though
   call print_matrix_int('dim=1',product(box,dim=1))

   call print_matrix_int('dim=2',product(box,dim=2))

   call print_matrix_int('dim=3',product(box,dim=3))

endblock WITH_DIM

contains

   subroutine print_matrix_int(title,arr)
   implicit none

   !@(#) print small 2d integer arrays in row-column format

   character(len=*),intent(in)  :: title
   integer,intent(in)           :: arr(:,:)
   integer                      :: i
   character(len=:),allocatable :: biggest

      print all
      print all, trim(title),':(',shape(arr),')'  ! print title
      biggest='           '  ! make buffer to write integer into
      ! find how many characters to use for integers
      write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
      ! use this format to write a row
      biggest='(" > [",*(i'//trim(biggest)//':,","))'
      ! print one row of array at a time
      do i=1,size(arr,dim=1)
         write(*,fmt=biggest,advance='no')arr(i,:)
         write(*,'(" ]")')
      enddo

   end subroutine print_matrix_int

end program demo_product
```

Results:

```text
factorial of  1  is  1.000000
factorial of  2  is  2.000000
factorial of  3  is  6.000000
factorial of  4  is  24.00000
factorial of  5  is  120.0000
factorial of  6  is  720.0000
factorial of  7  is  5040.000
factorial of  8  is  40320.00
factorial of  9  is  362880.0
factorial of  10  is  3628800.

 product of all elements 351000000
 odd * even =
 4875 * 72000 = 351000000

zero-sized array=> 1
all elements have a false mask=> 1

rect :( 2 3 )
 > [  1,  2,  3 ]
 > [  4,  5,  6 ]
product of columns= 4 10 18
product of rows= 6 120

box 1 :( 2 3 )
 > [  1,  2,  3 ]
 > [  4,  5,  6 ]

box 2 :( 2 3 )
 > [  10,  20,  30 ]
 > [  40,  50,  60 ]

box 3 :( 2 3 )
 > [ -10, -20, -30 ]
 > [ -40, -50, -60 ]

box 4 :( 2 3 )
 > [   2,   4,   6 ]
 > [   8,  10,  12 ]
no dim gives a scalar .1719927E+26

negative values :( 3 4 )
 > [     1,     1,   400,     1 ]
 > [     1,     1,  1000,     1 ]
 > [     1,     1,  1800,     1 ]

dim=1 :( 3 4 )
 > [     4,   400,   400,    16 ]
 > [    10,  1000,  1000,    40 ]
 > [    18,  1800,  1800,    72 ]

dim=2 :( 2 4 )
 > [       6,    6000,   -6000,      48 ]
 > [     120,  120000, -120000,     960 ]

dim=3 :( 2 3 )
 > [    -200,   -3200,  -16200 ]
 > [  -51200, -125000, -259200 ]
```

### **Standard**

Fortran 95

### **See Also**

[**sum**(3)](#sum), note that an element by element multiplication is done
directly using the star character.

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## radix

### **Name**

**radix**(3) - \[NUMERIC MODEL\] Base of a model number

### **Synopsis**
```fortran
   result = radix(x)
```
```fortran
    integer function radix(x)

     TYPE(kind=**),intent(in) :: x(..)
```
### **Characteristics**

   - TYPE may be _real_ or _integer_
   - **x** may be scalar or an array

### **Description**

  **radix**(3) returns the base of the internal model representing the
  numeric entity **x**.

  In a positional numeral system, the radix or base is the number of
  unique digits, including the digit zero, used to represent numbers.

  This function helps to represent the internal computing model
  generically, but will be 2 (representing a binary machine) for any
  common platform for all the numeric types.

### **Options**

- **x**
  : used to identify the type of number to query.

### **Result**

  The returned value indicates what base is internally used to represent
  the type of numeric value **x** represents.

### **Examples**

Sample program:

```fortran
program demo_radix
implicit none
   print *, "The radix for the default integer kind is", radix(0)
   print *, "The radix for the default real kind is", radix(0.0)
   print *, "The radix for the doubleprecision real kind is", radix(0.0d0)
end program demo_radix
```
Results:

```text
    The radix for the default integer kind is           2
    The radix for the default real kind is           2
    The radix for the doubleprecision real kind is          2
```

### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## random_number

### **Name**

**random_number**(3) - \[MATHEMATICS:RANDOM\] Pseudo-random number

### **Synopsis**
```fortran
    call random_number(harvest)
```
```fortran
     subroutine random_number(harvest)

      real,intent(out) :: harvest(..)
```
### **Characteristics**

- **harvest** and the result are default _real_ variables

REAL X, Y (10, 10)
! Initialize X with a pseudorandom number
CALL RANDOM_NUMBER (HARVEST = X)
CALL RANDOM_NUMBER (Y)
! X and Y contain uniformly distributed random numbers

### **Description**

**random_number**(3) returns a single pseudorandom number or an array of
pseudorandom numbers from the uniform distribution over the range
0 \<= x \< 1.

### **Options**

- **harvest**
  : Shall be a scalar or an array of type _real_.

### **Examples**

Sample program:

```fortran
program demo_random_number
use, intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
integer, allocatable :: seed(:)
integer              :: n
integer              :: first,last
integer              :: i
integer              :: rand_int
integer,allocatable  :: count(:)
real(kind=dp)        :: rand_val
   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   first=1
   last=10
   allocate(count(last-first+1))
   ! To have a discrete uniform distribution on the integers
   ! [first, first+1, ..., last-1, last] carve the continuous
   ! distribution up into last+1-first equal sized chunks,
   ! mapping each chunk to an integer.
   !
   ! One way is:
   !   call random_number(rand_val)
   ! choose one from last-first+1 integers
   !   rand_int = first + FLOOR((last+1-first)*rand_val)
      count=0
      ! generate a lot of random integers from 1 to 10 and count them.
      ! with a large number of values you should get about the same
      ! number of each value
      do i=1,100000000
         call random_number(rand_val)
         rand_int=first+floor((last+1-first)*rand_val)
         if(rand_int.ge.first.and.rand_int.le.last)then
            count(rand_int)=count(rand_int)+1
         else
            write(*,*)rand_int,' is out of range'
         endif
      enddo
      write(*,'(i0,1x,i0)')(i,count(i),i=1,size(count))
end program demo_random_number
```
Results:
```
   1 10003588
   2 10000104
   3 10000169
   4 9997996
   5 9995349
   6 10001304
   7 10001909
   8 9999133
   9 10000252
   10 10000196
```
### **Standard**

Fortran 95

### **See Also**

[**random_seed**(3)](#random_seed)

 _fortran-lang intrinsic descriptions_

## random_seed

### **Name**

**random_seed**(3) - \[MATHEMATICS:RANDOM\] Initialize a pseudo-random number sequence

### **Synopsis**
```fortran
    call random_seed( [size] [,put] [,get] )
```
```fortran
     subroutine random_seed( size, put, get )

      integer,intent(out),optional :: size
      integer,intent(in),optional :: put(*)
      integer,intent(out),optional :: get(*)
```
### **Characteristics**

### **Description**

**random_seed**(3) restarts or queries the state of the pseudorandom
number generator used by random_number.

If random_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

### **Options**

- **size**
  : (Optional) Shall be a scalar and of type default _integer_, with
  **intent(out)**. It specifies the minimum size of the arrays used
  with the **put** and **get** arguments.

- **put**
  : (Optional) Shall be an array of type default _integer_ and rank one.
  It is **intent(in)** and the size of the array must be larger than
  or equal to the number returned by the **size** argument.

- **get**
  : (Optional) Shall be an array of type default _integer_ and rank one.
  It is **intent(out)** and the size of the array must be larger than
  or equal to the number returned by the **size** argument.

### **Examples**

Sample program:

```fortran
program demo_random_seed
implicit none
integer, allocatable :: seed(:)
integer :: n

   call random_seed(size = n)
   allocate(seed(n))
   call random_seed(get=seed)
   write (*, *) seed

end program demo_random_seed
```
Results:
```text
     -674862499 -1750483360  -183136071  -317862567   682500039
     349459   344020729 -1725483289
```
### **Standard**

Fortran 95

### **See Also**

[**random_number**(3)](#random_number)

 _fortran-lang intrinsic descriptions_

## range

### **Name**

**range**(3) - \[NUMERIC MODEL\] Decimal exponent range of a real kind

### **Synopsis**
```fortran
    result = range(x)
```
```fortran
      integer function range (x)

       TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

where TYPE is _real_ or _complex_ and KIND is any kind supported by
TYPE.

### **Description**

**range**(3) returns the decimal exponent range in the model of the type
of **x**.

### **Options**

- **x**
  : Shall be of type _real_ or _complex_.

### **Result**

The return value is of type _integer_ and of the default integer kind.

### **Examples**

Sample program:

```fortran
program demo_range
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp)    :: x(2)
complex(kind=dp) :: y
   print *, precision(x), range(x)
   print *, precision(y), range(y)
end program demo_range
```

Results:

```text
              6          37
             15         307
```

### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## rank

### **Name**

**rank**(3) - \[ARRAY INQUIRY\] Rank of a data object

### **Synopsis**
```fortran
    result = rank(a)
```
```fortran
     integer function rank(a)

      type(TYPE(kind=**)),intent(in) :: a(..)
```
### **Characteristics**

 -  **a** can be of any type **TYPE** and rank.
 - a kind designated as ** may be any supported kind value for the type

### **Description**

  **rank**(3) returns the rank of a scalar or array data object.

  The rank of an array is the number of dimensions it has (zero for a scalar).

### **Options**

- **a** is the data object to query the dimensionality of. The rank returned
  may be from 0 to 16.

  The argument **a** may be any data object type, including an assumed-rank
  array.

### **Result**

  For arrays, their rank is returned; for scalars zero is returned.

### **Examples**

Sample program:

```fortran
program demo_rank
implicit none

! a bunch of data objects to query
integer           :: a
real, allocatable :: b(:,:)
real, pointer     :: c(:)
complex           :: d

! make up a type
type mytype
   integer :: int
   real :: float
   character :: char
end type mytype
type(mytype) :: any_thing(1,2,3,4,5)

  ! basics
   print *, 'rank of scalar a=',rank(a)
   ! you can query this array even though it is not allocated
   print *, 'rank of matrix b=',rank(b)
   print *, 'rank of vector pointer c=',rank(c)
   print *, 'rank of complex scalar d=',rank(d)

  ! you can query any type, not just intrinsics
   print *, 'rank of any arbitrary type=',rank(any_thing)

  ! an assumed-rank object may be queried
   call query_int(10)
   call query_int([20,30])
   call query_int( reshape([40,50,60,70],[2,2]) )

  ! you can even query an unlimited polymorphic entity
   call query_anything(10.0)
   call query_anything([.true.,.false.])
   call query_anything( reshape([40.0,50.0,60.0,70.0],[2,2]) )

contains

subroutine query_int(data_object)
! It is hard to do much with something dimensioned
! name(..) if not calling C except inside of a
! SELECT_RANK construct but one thing you can
! do is call the inquiry functions ...
integer,intent(in) :: data_object(..)
character(len=*),parameter :: all='(*(g0,1x))'

   if(rank(data_object).eq.0)then
      print all,&
      & 'passed a scalar to an assumed rank,  &
      & rank=',rank(data_object)
   else
      print all,&
      & 'passed an array to an assumed rank,  &
      & rank=',rank(data_object)
   endif

end subroutine query_int

subroutine query_anything(data_object)
class(*),intent(in) ::data_object(..)
character(len=*),parameter :: all='(*(g0,1x))'
  if(rank(data_object).eq.0)then
    print all,&
    &'passed a scalar to an unlimited polymorphic rank=', &
    & rank(data_object)
  else
    print all,&
    & 'passed an array to an unlimited polymorphic, rank=', &
    & rank(data_object)
  endif
end subroutine query_anything

end program demo_rank
```
Results:
```text
    rank of scalar a=           0
    rank of matrix b=           2
    rank of vector pointer c=           1
    rank of complex scalar d=           0
    rank of any arbitrary type=           5
   passed a scalar to an assumed rank,   rank= 0
   passed an array to an assumed rank,   rank= 1
   passed an array to an assumed rank,   rank= 2
   passed a scalar to an unlimited polymorphic rank= 0
   passed an array to an unlimited polymorphic, rank= 1
   passed an array to an unlimited polymorphic, rank= 2
```
### **Standard**

### **See also**

#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contigious) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry:

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## real

### **Name**

**real**(3) - \[TYPE:NUMERIC\] Convert to real type

### **Synopsis**
```fortran
  result = real(x [,kind])
```
```fortran
   elemental real(kind=KIND) function real(x,kind)

    TYPE(kind=**),intent(in) :: x
    integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
 - the type of **x** **TYPE** may be _integer_, _real_, or _complex_.
 - **kind** is a _integer_ initialization expression (a constant expression)
   + If **kind** is present it defines the kind of the result
   + if **kind** is not present
     - when **x** is complex the result is the same kind as **x**.
     - when **x** is real or integer the result is a _real_ of default kind

### **Description**

**real**(3) converts its argument **x** to a _real_ type.

For complex values this is similar to the modern complex-part-designator
**%RE** which also designates the real part of _complex_ a value.
```fortran
      z=(3.0,4.0)     ! if z is a complex value
      print *, z%re == real(z) ! these expressions are equivalent
```
### **Options**

- **x**
  : A _integer_, _real_, or _complex_ value to convert to _real_.

- **kind**
  : When present the value of **kind** defines the kind of the result.

### **Result**

1.  **real(x)** converts **x** to a default _real_ type if **x** is an _integer_
    or _real_ variable.

2.  **real(x)** converts a _complex_ value to a _real_ type with the
    magnitude of the real component of the input with kind type
    parameter the same as **x**.

3.  **real(x, kind)** is converted to a _real_ type with kind type
    parameter **kind** if **x** is a _complex_, _integer_, or _real_ variable.

### **Examples**

Sample program:
```fortran
program demo_real
use,intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
complex              :: zr = (1.0, 2.0)
doubleprecision      :: xd=huge(3.0d0)
complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)

   print *, real(zr), aimag(zr)
   print *, dble(zd), aimag(zd)

   write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)
end program demo_real
```
Results:
```
 1.00000000       2.00000000
 4.0000000000000000       5.0000000000000000
 1.7976931348623157E+308  1.7976931348623157E+308  1.7976931348623157E+308
```
### **Standard**

FORTRAN 77

### **See Also**

- [**aimag**(3)](#aimag) - Imaginary part of complex number
- [**cmplx**(3)](#cmplx) - Complex conversion function
- [**conjg**(3)](#conjg) - Complex conjugate function

Fortran has strong support for _complex_ values, including many intrinsics
that take or produce _complex_ values in addition to algebraic and
logical expressions:

[**abs**(3)](#abs),
[**acosh**(3)](#acosh),
[**acos**(3)](#acos),
[**asinh**(3)](#asinh),
[**asin**(3)](#asin),
[**atan2**(3)](#atan2),
[**atanh**(3)](#atanh),
[**atan**(3)](#atan),
[**cosh**(3)](#cosh),
[**cos**(3)](#cos),
[**co_sum**(3)](#co_sum),
[**dble**(3)](#dble),
[**dot_product**(3)](#dot_product),
[**exp**(3)](#exp),
[**int**(3)](#int),
[**is_contiguous**(3)](#is_contiguous),
[**kind**(3)](#kind),
[**log**(3)](#log),
[**matmul**(3)](#matmul),
[**precision**(3)](#precision),
[**product**(3)](#product),
[**range**(3)](#range),
[**rank**(3)](#rank),
[**sinh**(3)](#sinh),
[**sin**(3)](#sin),
[**sqrt**(3)](#sqrt),
[**storage_size**(3)](#storage_size),
[**sum**(3)](#sum),
[**tanh**(3)](#tanh),
[**tan**(3)](#tan),
[**unpack**(3)](#unpack),

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## reduce

### **Name**

**reduce**(3) - \[TRANSFORMATIONAL\] General reduction of an array

### **Synopsis**

There are two forms to this function:
```fortran
   result = reduce(array, operation [,mask]  [,identity]  [,ordered] )
```
or
```fortran
   result = reduce (array, operation &
   & [,dim] [,mask] [,identity] [,ordered] )
```
```fortran
    type(TYPE(kind=KIND)) function reduce &
    & (array, operation, dim, mask, identity, ordered )

     type(TYPE(kind=KIND)),intent(in) :: array
     pure function                  :: operation
     integer,intent(in),optional    :: dim
     logical,optional               :: mask
     type(TYPE),intent(in),optional :: identity
     logical,intent(in),optional    :: ordered
```
### **Characteristics**

   where TYPE may be of any type. TYPE must be the same for **array**
   and **identity**.

### **Description**

   **reduce**(3) reduces a list of conditionally selected values from
   an array to a single value by iteratively applying a binary function.

   Common in functional programming, a **reduce** function applies a
   binary operator (a pure function with two arguments) to all elements
   cumulatively.

   **reduce** is a "higher-order" function; ie. it is a function that
   receives other functions as arguments.

   The **reduce** function receives a binary operator (a function with
   two arguments, just like the basic arithmetic operators). It is first
   applied to two unused values in the list to generate an accumulator
   value which is subsequently used as the first argument to the function
   as the function is recursively applied to all the remaining selected
   values in the input array.

### **Options**

- **array**
  : An array of any type and allowed rank to select values from.

- **operation**
  : shall be a pure function with exactly two arguments;
  each argument shall be a scalar, nonallocatable,
  nonpointer, nonpolymorphic, nonoptional dummy data object
  with the same type and type parameters as **array**. If
  one argument has the ASYNCHRONOUS, TARGET, or VALUE
  attribute, the other shall have that attribute. Its result
  shall be a nonpolymorphic scalar and have the same type
  and type parameters as **array**. **operation** should
  implement a mathematically associative operation. It
  need not be commutative.

  NOTE

  If **operation** is not computationally associative, REDUCE
  without ORDERED=.TRUE. with the same argument values
  might not always produce the same result, as the processor
  can apply the associative law to the evaluation.

  Many operations that mathematically are associative are
  not when applied to floating-point numbers. The order
  you sum values in may affect the result, for example.

- **dim**
  : An integer scalar with a value in the range
  1<= **dim** <= n, where n is the rank of **array**.

- **mask**
  : (optional) shall be of type logical and shall be
  conformable with **array**.

  When present only those elements of **array** are passed
  to **operation** for which the corresponding elements
  of **mask** are true, as if **array* was filtered with
  **pack(3)**.

- **identity**
  : shall be scalar with the same type and type parameters as **array**.
  If the initial sequence is empty, the result has the value **identify**
  if **identify** is present, and otherwise, error termination is
  initiated.

- **ordered**
  : shall be a logical scalar. If **ordered** is present with the value
  _.true._, the calls to the **operator** function begins with the first
  two elements of **array** and the process continues in row-column
  order until the sequence has only one element which is the value of the
  reduction. Otherwise, the compiler is free to assume that the operation
  is commutative and may evaluate the reduction in the most optimal way.

### **Result**

The result is of the same type and type parameters as **array**. It is
scalar if **dim** does not appear.

If **dim** is present, it indicates the one dimension along which to
perform the reduction, and the resultant array has a rank reduced by
one relative to the input array.

### **Examples**

   The following examples all use the function MY_MULT, which returns
   the product of its two real arguments.
```fortran
   program demo_reduce
   implicit none
   character(len=*),parameter :: f='("[",*(g0,",",1x),"]")'
   integer,allocatable :: arr(:), b(:,:)

   ! Basic usage:
      ! the product of the elements of an array
      arr=[1, 2, 3, 4 ]
      write(*,*) arr
      write(*,*) 'product=', reduce(arr, my_mult)
      write(*,*) 'sum=', reduce(arr, my_sum)

   ! Examples of masking:
      ! the product of only the positive elements of an array
      arr=[1, -1, 2, -2, 3, -3 ]
      write(*,*)'positive value product=',reduce(arr, my_mult, mask=arr>0)
   ! sum values ignoring negative values
      write(*,*)'sum positive values=',reduce(arr, my_sum, mask=arr>0)

   ! a single-valued array returns the single value as the
   ! calls to the operator stop when only one element remains
      arr=[ 1234 ]
      write(*,*)'single value sum',reduce(arr, my_sum )
      write(*,*)'single value product',reduce(arr, my_mult )

   ! Example of operations along a dimension:
   !  If B is the array   1 3 5
   !                      2 4 6
      b=reshape([1,2,3,4,5,6],[2,3])
      write(*,f) REDUCE(B, MY_MULT),'should be [720]'
      write(*,f) REDUCE(B, MY_MULT, DIM=1),'should be [2,12,30]'
      write(*,f) REDUCE(B, MY_MULT, DIM=2),'should be [15, 48]'

   contains

   pure function my_mult(a,b) result(c)
   integer,intent(in) :: a, b
   integer            :: c
      c=a*b
   end function my_mult

   pure function my_sum(a,b) result(c)
   integer,intent(in) :: a, b
   integer            :: c
      c=a+b
   end function my_sum

   end program demo_reduce
```
Results:
```text
     >  1 2 3 4
     >  product= 24
     >  sum=     10
     >  positive value sum= 6
     >  sum positive values= 6
     >  single value sum     1234
     >  single value product 1234
     > [720, should be [720],
     > [2, 12, 30, should be [2,12,30],
     > [15, 48, should be [15, 48],
````

### **Standard**

   Fortran 2018

### **See Also**
- [co_reduce(3)](#co_reduce)

### **Resources**

- [associative:wikipedia](https://en.wikipedia.org/wiki/Associative_property)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## repeat

### **Name**

**repeat**(3) - \[CHARACTER\] Repeated string concatenation

### **Synopsis**
```fortran
    result = repeat(string, ncopies)
```
```fortran
     character(len=len(string)*ncopies) function repeat(string, ncopies)

      character(len=*),intent(in)   :: string
      integer(kind=**),intent(in)   :: ncopies
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **string** is a scalar _character_ type.
- **ncopies** is a scalar integer.
- the result is a new scalar of type _character_ of the same type as
  **string**

### **Description**

  **repeat**(3) concatenates copies of a string.

### **Options**

- **string**
  : The input string to repeat

- **ncopies**
  : Number of copies to make of **string**, greater than or equal to zero (0).

### **Result**

  A new string built up from **ncopies** copies of **string**.

### **Examples**

Sample program:
```fortran
program demo_repeat
implicit none
integer :: i
    write(*,'(a)') repeat("^v", 36)         ! line break
    write(*,'(a)') repeat("_", 72)          ! line break
    write(*,'(a)') repeat("1234567890", 7)  ! number line
    write(*,'(a)') repeat("         |", 7)  !
end program demo_repeat
```
Results:
```text
   ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
   ________________________________________________________________________
   1234567890123456789012345678901234567890123456789012345678901234567890
            |         |         |         |         |         |         |
```
### **Standard**

Fortran 95

### **See Also**

Functions that perform operations on character strings:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),
  [**scan**(3)](#scan),
  [**verify**(3)](#verify)

- **Non-elemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
#

## reshape

### **Name**

**reshape**(3) - \[ARRAY RESHAPE\] Function to reshape an array

### **Synopsis**
```fortran
    result = reshape( source, shape [,pad] [,order] )
```
```fortran
     type(TYPE(kind=KIND) function reshape

      type(TYPE(kind=KIND),intent(in)          :: source(..)
      integer(kind=**),intent(in)              :: shape(:)
      type(TYPE(kind=KIND),intent(in),optional :: pad(..)
      integer(kind=**),intent(in),optional     :: order(:)
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
 - **source** is an array of any type
 - **shape** defines a Fortran shape and therefore an _integer_ vector
   (of rank one) of constant size of up to 16 non-negative values.
 - **pad** is the same type as **source**
 - **order** is the same shape as **shape**
 - The result is an array of shape **shape** with the same type as **source**.

### **Description**

**reshape** constructs an array of shape **shape** using the elements
from **source** and possibly **pad** to fill it.

If necessary, the new array may be padded with elements from **pad**
or permuted as defined by **order**.

Among many other uses, **reshape** can be used to reorder a Fortran array
to match C array ordering before the array is passed from Fortran to a
C procedure.

### **Options**

- **source**
  : an array containing the elements to be copied to the result.
  there must be enough elements in the source to fill the new shape
  if **pad** is omitted or has size zero. Expressed in Fortran ...
```fortran
   if(.not.present(pad))then
      if(size(source) < product(shape))then
        stop 'not enough elements in the old array to fill the new one'
      endif
   endif
```
- **shape**
  : This is the shape of the new array being generated.
    Being by definition a shape; all elements are either positive integers
    or zero, the size but be 1 or greater, it may have up to 16 elements
    but must be of constant fixed size and rank one.

- **pad**
  : used to fill in extra values if the result array is larger than **source**.
    It will be used repeatedly after all the elements of **source** have been
    placed in the result until the result has all elements assigned.
  : If it is absent or is a zero-sized array, you can only make
    **ource** into another array of the same size as **source** or smaller.

- **order**
  : used to insert elements in the result in an order other
    than the normal Fortran array element order, in which the first dimension
    varies fastest.
  : By definition of ranks the values have to be a permutation of the numbers
    from 1 to n, where n is the rank of **shape**.
  : the elements of **source** and pad are placed into the result in order;
    changing the left-most rank most rapidly by default. To change the order by
    which the elements are placed in the result use **order**.

### **Result**

The result is an array of shape **shape** with the same type and type
parameters as **source**.  It is first filled with the values of elements
of **source**, with the remainder filled with repeated copies of **pad**
until all elements are filled. The new array may be smaller than
**source**.

### **Examples**

Sample program:
```fortran
program demo_reshape
implicit none
integer :: i
! notice the use of "shape(box)" on the RHS
integer :: box(3,4)=reshape([1,2,3,4,5,6,7,8,9,10,11,12],shape(box))
integer,allocatable :: v(:,:)
integer :: rc(2)
   ! basics0
    ! what is the current shape of the array?
    call printi('shape of box is ',box)
    ! change the shape
    call printi('reshaped ',reshape(box,[2,6]))
    call printi('reshaped ',reshape(box,[4,3]))

   ! fill in row column order using order
    v=reshape([1,2,3,4,10,20,30,40,100,200,300,400],[1,12])
    call printi('here is some data to shape',v)
    call printi('normally fills columns first ',reshape([v],[3,4]))
    call printi('fill rows first', reshape([v],[3,4],order=[2,1]))

    ! if we take the data and put in back in filling
    ! rows first instead of columns, and flipping the
    ! height and width of the box we not only fill in
    ! a vector using row-column order we actually
    ! transpose it.
    rc(2:1:-1)=shape(box)
    ! copy the data in changing column number fastest
    v=reshape(box,rc,order=[2,1])
    call printi('reshaped and reordered',v)
    ! of course we could have just done a transpose
    call printi('transposed',transpose(box))

   ! making the result bigger than source using pad
    v=reshape(box,rc*2,pad=[-1,-2,-3],order=[2,1])
    call printi('bigger and padded and reordered',v)
contains

subroutine printi(title,arr)
implicit none

!@(#) print small 2d integer arrays in row-column format

character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest

   print all
   print all, trim(title),':(',shape(arr),')'  ! print title
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo

end subroutine printi

end program demo_reshape
```
Results:
```text
   shape of box is :( 3 4 )
    > [   1,   4,   7,  10 ]
    > [   2,   5,   8,  11 ]
    > [   3,   6,   9,  12 ]

   reshaped :( 2 6 )
    > [   1,   3,   5,   7,   9,  11 ]
    > [   2,   4,   6,   8,  10,  12 ]

   reshaped :( 4 3 )
    > [   1,   5,   9 ]
    > [   2,   6,  10 ]
    > [   3,   7,  11 ]
    > [   4,   8,  12 ]

   here is some data to shape :( 1 12 )
    > [   1,   2,   3,   4,  10,  20,  30,  40, 100, 200, 300, 400 ]

   normally fills columns first :( 3 4 )
    > [    1,    4,   30,  200 ]
    > [    2,   10,   40,  300 ]
    > [    3,   20,  100,  400 ]

   fill rows first :( 3 4 )
    > [    1,    2,    3,    4 ]
    > [   10,   20,   30,   40 ]
    > [  100,  200,  300,  400 ]

   reshaped and reordered :( 4 3 )
    > [   1,   2,   3 ]
    > [   4,   5,   6 ]
    > [   7,   8,   9 ]
    > [  10,  11,  12 ]

   transposed :( 4 3 )
    > [   1,   2,   3 ]
    > [   4,   5,   6 ]
    > [   7,   8,   9 ]
    > [  10,  11,  12 ]

   bigger and padded and reordered :( 8 6 )
    > [   1,   2,   3,   4,   5,   6 ]
    > [   7,   8,   9,  10,  11,  12 ]
    > [  -1,  -2,  -3,  -1,  -2,  -3 ]
    > [  -1,  -2,  -3,  -1,  -2,  -3 ]
    > [  -1,  -2,  -3,  -1,  -2,  -3 ]
    > [  -1,  -2,  -3,  -1,  -2,  -3 ]
    > [  -1,  -2,  -3,  -1,  -2,  -3 ]
    > [  -1,  -2,  -3,  -1,  -2,  -3 ]
```
### **Standard**

Fortran 95

### **See Also**

[**shape**(3)](#shape),
[**pack**(3)](#pack),
[**transpose**(3)](#transpose)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_


## rrspacing

### **Name**

**rrspacing**(3) - \[MODEL_COMPONENTS\] Reciprocal of the relative spacing

### **Synopsis**
```fortran
    result = rrspacing(x)
```
```fortran
     elemental real(kind=KIND) function rrspacing(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

The return value is of the same type and kind as **x**.

### **Description**

**rrspacing**(3) returns the reciprocal of the relative spacing of model
numbers near **x**.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The return value is of the same type and kind as **x**. The value returned
is equal to **abs(fraction(x)) \* float(radix(x))\*\*digits(x)**.

### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## same_type_as

### **Name**

**same_type_as**(3) - \[STATE\] Query dynamic types for equality

### **Synopsis**
```fortran
    result = same_type_as(a, b)
```
```fortran
     logical same_type_as(a, b)

      type(TYPE(kind=KIND),intent(in) :: a
      type(TYPE(kind=KIND),intent(in) :: b
```
### **Characteristics**

- **a** shall be an object of extensible declared type or unlimited
  polymorphic. If it is a polymorphic pointer, it shall not have
  an undefined association status.

- **b** shall be an object of extensible declared type or unlimited
  polymorphic. If it is a polymorphic pointer, it shall not have
  an undefined association status.

### **Description**

**same_type_as**(3) queries the dynamic types of objects for equality.

### **Options**

- **a**
  : object to compare to **b** for equality of type

- **b**
  : object to be compared to for equality of type

### **Result**

  If the dynamic type of **a** or **b** is extensible, the result is true
  if and only if the dynamic type of **a** is the same as the dynamic
  type of **b**. If neither **a** nor **b** has extensible dynamic type,
  the result is processor dependent.

    NOTE1

  The dynamic type of a disassociated pointer or unallocated allocatable
  variable is its declared type. An unlimited polymorphic entity has no
  declared type.

    NOTE2

  The test performed by SAME_TYPE_AS is not the same as the test performed
  by the type guard TYPE IS. The test performed by SAME_TYPE_AS does
  not consider kind type parameters.

Sample program:
```fortran
  ! program demo_same_type_as
  module M_ether

  type   :: dot
    real :: x=0
    real :: y=0
  end type dot

  type, extends(dot) :: point
    real :: z=0
  end type point

  type something_else
  end type something_else

  end module M_ether

  program demo_same_type_as
  use M_ether, only : dot, point, something_else
  type(dot) :: dad, mom
  type(point) :: me
  type(something_else) :: alien

   write(*,*)same_type_as(me,dad),'I am descended from Dad, but equal?'
   write(*,*)same_type_as(me,me) ,'I am what I am'
   write(*,*)same_type_as(dad,mom) ,'what a pair!'

   write(*,*)same_type_as(dad,me),'no paradox here'
   write(*,*)same_type_as(dad,alien),'no relation'

   call pointers()
   contains
   subroutine pointers()
   ! Given the declarations and assignments
   type t1
      real c
   end type
   type, extends(t1) :: t2
   end type
   class(t1), pointer :: p, q, r
      allocate (p, q)
      allocate (t2 :: r)
      ! the result of SAME_TYPE_AS (P, Q) will be true, and the result
      ! of SAME_TYPE_AS (P, R) will be false.
      write(*,*)'(P,Q)',same_type_as(p,q),"mind your P's and Q's"
      write(*,*)'(P,R)',same_type_as(p,r)
   end subroutine pointers

  end program demo_same_type_as
```
Results:
```text
    F I am descended from Dad, but equal?
    T I am what I am
    T what a pair!
    F no paradox here
    F no relation
    (P,Q) T mind your P's and Q's
    (P,R) F
```
### **Standard**

Fortran 2003

### **See Also**

[**extends_type_of**(3)](#extends_type_of)

 _fortran-lang intrinsic descriptions_

## scale

### **Name**

**scale**(3) - \[MODEL_COMPONENTS\] Scale a real value by a whole power of the radix

### **Synopsis**
```fortran
    result = scale(x, i)
```
```fortran
     elemental real(kind=KIND) function scale(x, i)

      real(kind=KIND),intent(in) :: x
      integer,intent(in)         :: i
```
### **Characteristics**

### **Description**

**scale**(3) returns x \* **radix(x)\*\*i**.

### **Options**

- **x**
  : The type of the argument shall be a _real_.

- **i**
  : The type of the argument shall be a _integer_.

### **Result**

The return value is of the same type and kind as **x**. Its value is
**x \* radix(x)\*\*i**.

### **Examples**

Sample program:
```fortran
program demo_scale
implicit none
real :: x = 178.1387e-4
integer :: i = 5
   print *, scale(x,i), x*radix(x)**i
end program demo_scale
```
Results:
```
    0.570043862      0.570043862
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## scan

### **Name**

**scan**(3) - \[CHARACTER:SEARCH\] Scan a string for the presence of a set of characters

### **Synopsis**
```fortran
    result = scan( string, set, [,back] [,kind] )
```
```fortran
     elemental integer(kind=KIND) function scan(string,set,back,kind)

      character(len=*,kind=**),intent(in) :: string
      character(len=*,kind=**),intent(in) :: set
      logical,intent(in),optional :: back
      integer,intent(in),optional :: kind
```
### **Characteristics**

**string** and **set**  must have the same kind type parameter.

the kind of the returned value is the same as **kind** if
present. Otherwise a default _integer_ kind is returned.

### **Description**

**scan**(3) scans a **string** for any of the characters in a **set**
of characters.

If **back** is either absent or equals _.false._, this function
returns the position of the leftmost character of **STRING** that is
in **set**. If **back** equals _.true._, the rightmost position is
returned. If no character of **set** is found in **string**, the result
is zero.

### **Options**

- **string**
  : Shall be of type _character_.

- **set**
  : Shall be of type _character_.

- **back**
  : (Optional) shall be of type _logical_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind.

### **Examples**

Sample program:

```fortran
program demo_scan
implicit none
   write(*,*) scan("fortran", "ao")          ! 2, found 'o'
   write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
   write(*,*) scan("fortran", "c++")         ! 0, found none
end program demo_scan
```

Results:

```text
              2
              6
              0
```

### **Standard**

Fortran 95 , with KIND argument - Fortran 2003

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl), [**adjustr**(3)](#adjustr), [**index**(3)](#index),
  [**scan**(3)](#scan), [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat), [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions_

## selected_char_kind

### **Name**

**selected_char_kind**(3) - \[KIND\] Select character kind such as "Unicode"

### **Synopsis**
```fortran
    result = selected_char_kind(name)
```
```fortran
     integer function selected_char_kind(name)

      character(len=*),intent(in) :: name
```
### **Characteristics**

### **Description**

  **selected_char_kind**(3) returns the kind value for the character
  set named NAME, if a character set with such a name is supported, or
  **-1** otherwise.

### **Options**

- **name**
  : A name to query the processor kind value of , and/or to determine
  if it is supported. **name** is interpreted without respect to case
  or trailing blanks.

  Currently, supported character sets include "ASCII" and "DEFAULT" and
  "ISO_10646" (Universal Character Set, UCS-4) which is commonly known as
  "Unicode". Supported names other than "DEFAULT" are processor dependent.

### **Result**

If a name is not supported, -1 is returned. Otherwise

 + If NAME has the value "DEFAULT", then the result has a value equal to
   that of the kind type parameter of default character. This name is
   always supported.

 + If NAME has the value "ASCII", then the result has a value equal
   to that of the kind type parameter of ASCII character.

 + If NAME has the value "ISO_10646", then the result has a value equal
   to that of the kind type parameter of the ISO 10646 character kind
   (corresponding to UCS-4 as specified in ISO/IEC 10646).

 + If NAME is a processor-defined name of some other character kind
   supported by the processor, then the result has a value equal to that
   kind type parameter value.

### **Examples**

Sample program:

```fortran
program demo_selected_char_kind
use iso_fortran_env
implicit none

intrinsic date_and_time,selected_char_kind

integer, parameter :: default = selected_char_kind ("default")
integer, parameter :: ascii =   selected_char_kind ("ascii")
integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')
integer, parameter :: utf8  =   selected_char_kind ('utf-8')

character(len=26, kind=ascii ) :: alphabet
character(len=30, kind=ucs4  ) :: hello_world
character(len=30, kind=ucs4  ) :: string

   write(*,*)'ASCII     ',&
    & merge('SUPPORTED    ','NOT SUPPORTED',ascii /= -1)
   write(*,*)'ISO_10646 ',&
    & merge('SUPPORTED    ','NOT SUPPORTED',ucs4 /= -1)
   write(*,*)'UTF-8     ',&
    & merge('SUPPORTED    ','NOT SUPPORTED',utf8 /= -1)

   if(default.eq.ascii)then
       write(*,*)'ASCII is the default on this processor'
   endif

  ! the kind precedes the value, somewhat like a BOZ constant
   alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
   write (*,*) alphabet

   hello_world = ucs4_'Hello World and Ni Hao -- ' &
                 // char (int (z'4F60'), ucs4)     &
                 // char (int (z'597D'), ucs4)

  ! an  encoding option is required on OPEN for non-default I/O
   if(ucs4 /= -1 )then
      open (output_unit, encoding='UTF-8')
      write (*,*) trim (hello_world)
   else
      write (*,*) 'cannot use utf-8'
   endif

   call create_date_string(string)
   write (*,*) trim (string)

contains

! The following produces a Japanese date stamp.
subroutine create_date_string(string)
intrinsic date_and_time,selected_char_kind
integer,parameter :: ucs4 = selected_char_kind("ISO_10646")
character(len=1,kind=ucs4),parameter :: &
       nen =   char(int( z'5e74' ),ucs4), & ! year
       gatsu = char(int( z'6708' ),ucs4), & ! month
       nichi = char(int( z'65e5' ),ucs4)    ! day
     character(len= *, kind= ucs4) string
     integer values(8)
     call date_and_time(values=values)
     write(string,1) values(1),nen,values(2),gatsu,values(3),nichi
   1 format(i0,a,i0,a,i0,a)
end subroutine create_date_string

end program demo_selected_char_kind
```
Results:

The results are very processor-dependent
```text
 ASCII     SUPPORTED
 ISO_10646 SUPPORTED
 UTF-8     NOT SUPPORTED
 ASCII is the default on this processor
 abcdefghijklmnopqrstuvwxyz
 Hello World and Ni Hao -- 你好
 2022年10月5日
```
### **Standard**

Fortran 2003

### **See also**

[**achar**(3)](#achar)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## selected_int_kind

### **Name**

**selected_int_kind**(3) - \[KIND\] Choose integer kind

### **Synopsis**
```fortran
    result = selected_int_kind(r)
```
```fortran
    integer function selected_int_kind(r)

     integer(kind=KIND),intent(in) :: r
```
### **Characteristics**

### **Description**

**selected_int_kind**(3) return the kind value of the smallest integer
type that can represent all values ranging from **-10\*\*r** (exclusive)
to **10\*\*r** (exclusive). If there is no integer kind that accommodates
this range, selected_int_kind returns **-1**.

### **Options**

- **r**
  : Shall be a scalar and of type _integer_.

### **Examples**

Sample program:

```fortran
program demo_selected_int_kind
implicit none
integer,parameter :: k5 = selected_int_kind(5)
integer,parameter :: k15 = selected_int_kind(15)
integer(kind=k5) :: i5
integer(kind=k15) :: i15

    print *, huge(i5), huge(i15)

    ! the following inequalities are always true
    print *, huge(i5) >= 10_k5**5-1
    print *, huge(i15) >= 10_k15**15-1
end program demo_selected_int_kind
```

Results:

```text
     2147483647  9223372036854775807
    T
    T
```

### **Standard**

Fortran 95

### **See Also**

[**aint**(3)](#aint),
[**anint**(3)](#anint),
[**int**(3)](#int),
[**nint**(3)](#nint),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _fortran-lang intrinsic descriptions_

## selected_real_kind

### **Name**

**selected_real_kind**(3) - \[KIND\] Choose real kind

### **Synopsis**
```fortran
    result = selected_real_kind([p] [,r] [,radix] )
```
```fortran
    integer function selected_int_kind(r)

     real(kind=KIND),intent(in),optional :: p
     real(kind=KIND),intent(in),optional :: r
     real(kind=KIND),intent(in),optional :: radix
```
### **Characteristics**

### **Description**

**selected_real_kind**(3) return the kind value of a real data type with
decimal precision of at least **p** digits, exponent range of at least
**r**, and with a radix of **radix**.

### **Options**

- **p**
  : shall be a scalar and of type _integer_.

- **r**
  : shall be a scalar and of type _integer_.

- **radix**
  : shall be a scalar and of type _integer_.

Before **Fortran 2008**, at least one of the arguments **r** or **p** shall
be present; since **Fortran 2008**, they are assumed to be zero if
absent.

### **Result**

selected_real_kind returns the value of the kind type parameter of a
real data type with decimal precision of at least **p** digits, a decimal
exponent range of at least R, and with the requested **radix**. If the **radix**
parameter is absent, real kinds with any radix can be returned. If more
than one real data type meet the criteria, the kind of the data type
with the smallest decimal precision is returned. If no real data type
matches the criteria, the result is

  - **-1**
  : if the processor does not support a real data type with a
    precision greater than or equal to **p**, but the **r** and **radix**
    requirements can be fulfilled

  - **-2**
  : if the processor does not support a real type with an
    exponent range greater than or equal to **r**, but **p** and **radix** are
    fulfillable

  - **-3**
  : if **radix** but not **p** and **r** requirements are fulfillable

  - **-4**
  : if **radix** and either **p** or **r** requirements are fulfillable

  - **-5**
  : if there is no real type with the given **radix**

### **Examples**

Sample program:

```fortran
program demo_selected_real_kind
implicit none
integer,parameter :: p6 = selected_real_kind(6)
integer,parameter :: p10r100 = selected_real_kind(10,100)
integer,parameter :: r400 = selected_real_kind(r=400)
real(kind=p6) :: x
real(kind=p10r100) :: y
real(kind=r400) :: z

   print *, precision(x), range(x)
   print *, precision(y), range(y)
   print *, precision(z), range(z)
end program demo_selected_real_kind
```

Results:

```text
              6          37
             15         307
             18        4931
```

### **Standard**

Fortran 95 ; with RADIX - Fortran 2008

### **See Also**

[**precision**(3)](#precision),
[**range**(3)](#range),
[**radix**(3)](#radix)

 _fortran-lang intrinsic descriptions_

## set_exponent

### **Name**

**set_exponent**(3) - \[MODEL_COMPONENTS\] Set the exponent of the model

### **Synopsis**
```fortran
    result = set_exponent(x, i)
```
```fortran
     elemental real(kind=KIND) function set_exponent(x,i)

      real(kind=KIND),intent(in) :: x
      integer(kind=**),intent(in) :: i
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

 - The return value is of the same type and kind as **x**.

### **Description**

**set_exponent**(3) returns the real number whose fractional part is
that of **x** and whose exponent part is **i**.

### **Options**

- **x**
  : Shall be of type _real_.

- **i**
  : Shall be of type _integer_.

### **Result**

The return value is of the same type and kind as **x**. The real number
whose fractional part is that that of **x** and whose exponent part if **i** is
returned; it is **fraction(x) \* radix(x)\*\*i**.

### **Examples**

Sample program:

```fortran
program demo_setexp
implicit none
real :: x = 178.1387e-4
integer :: i = 17
   print *, set_exponent(x, i), fraction(x) * radix(x)**i
end program demo_setexp
```

Results:

```text
      74716.7891       74716.7891
```

### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## shape

### **Name**

**shape**(3) - \[ARRAY INQUIRY\] Determine the shape of an array

### **Synopsis**
```fortran
  result = shape(source [,kind])
```
```fortran
   integer(kind=KIND) function(shape(source,kind)

    type(TYPE(kind=**)),intent(in)        :: source(..)
    integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

  - **source** is an array or scalar of any type. If **source** is a pointer
    it must be associated and allocatable arrays must be allocated.

### **Description**

**shape**(3) determines the shape of an array.

### **Options**

- **source**
  : Shall be an array or scalar of any type. If **source** is a pointer it
  must be associated and allocatable arrays must be allocated.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

An _integer_ array of rank one with as many elements as **source** has
dimensions. The elements of the resulting array correspond to the extend
of **source** along the respective dimensions. If **source** is a scalar, the
result is the rank one array of size zero. If **kind** is absent, the return
value has the default integer kind otherwise the specified kind.

### **Examples**

Sample program:

```fortran
program demo_shape
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
integer, dimension(-1:1, -1:2) :: a
   print all, 'shape of array=',shape(a)
   print all, 'shape of constant=',shape(42)
   print all, 'size of shape of constant=',size(shape(42))
   print all, 'ubound of array=',ubound(a)
   print all, 'lbound of array=',lbound(a)
end program demo_shape
```

Results:

```text
   shape of array= 3 4
   shape of constant=
   size of shape of constant= 0
   ubound of array= 1 2
   lbound of array= -1 -1
```

### **Standard**

Fortran 95 ; with KIND argument Fortran 2003

### **See Also**

#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contigious) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry:

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## shifta

### **Name**

**shifta**(3) - \[BIT:SHIFT\] Shift bits right with fill

### **Synopsis**
```fortran
    result = shifta(i, shift )
```
```fortran
     elemental integer(kind=KIND) function shifta(i, shift)

      integer(kind=KIND),intent(in) :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
```
### **Characteristics**

  where KIND and SHIFTKIND may be any supported _integer_ kind, but
  where the kind for **i** dictates the kind of the returned value.

### **Description**

  **shifta**(3) returns a value corresponding to **i** with all of the
  bits shifted right by **shift** places and the vacated bits on the
  left filled with the value of the original left-most bit..

### **Options**

- **i**
  : The initial value to shift and fill

- **shift**
  : how many bits to shift right.
    It shall be nonnegative and less than or equal to **bit_size(i)**.
    or the value is undefined.

### **Result**

  The result characteristics (kind, type, rank, shape, ....) are the
  same as **i**.

  The result has the value obtained by shifting the bits of **i** to
  the right **shift**  bits and replicating the leftmost bit of **i**
  in the left **shift** bits (the leftmost bit in "two's complement"
  representation is the sign bit).

  Bits shifted out from the right end are lost.

  If **shift** is zero the result is **i**.

### **Examples**

Sample program:
```fortran
program demo_shifta
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer(kind=int32) :: ival
integer             :: shift
integer(kind=int32) :: oval
integer(kind=int32),allocatable :: ivals(:)
integer             :: i
integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])

  ! basic usage
  write(*,*)shifta(100,3)

  ! loop through some interesting values
   shift=5

   ivals=[ -1, -0, +0, +1, &
   & int(b"01010101010101010101010101010101"), &
   & int(b"10101010101010101010101010101010"), &
   & int(b"00000000000000000000000000011111") ]

   ! does your platform distinguish between +0 and -0?
   ! note the original leftmost bit is used to fill in the vacated bits

   write(*,'(/,"SHIFT =  ",i0)') shift
   do i=1,size(ivals)
      ival=ivals(i)
      write(*,'(  "I =      ",b32.32," == ",i0)') ival,ival
      oval=shifta(ival,shift)
      write(*,'(  "RESULT = ",b32.32," == ",i0)') oval,oval
   enddo
   ! elemental
   write(*,*)"characteristics of the result are the same as input"
   write(*,'(*(g0,1x))') &
     & "kind=",kind(shifta(arr,3)), "shape=",shape(shifta(arr,3)), &
     & "size=",size(shifta(arr,3)) !, "rank=",rank(shifta(arr,3))

end program demo_shifta
```
Results:

```text
 >           12
 >
 > SHIFT =  5
 > I =      11111111111111111111111111111111 == -1
 > RESULT = 11111111111111111111111111111111 == -1
 > I =      00000000000000000000000000000000 == 0
 > RESULT = 00000000000000000000000000000000 == 0
 > I =      00000000000000000000000000000000 == 0
 > RESULT = 00000000000000000000000000000000 == 0
 > I =      00000000000000000000000000000001 == 1
 > RESULT = 00000000000000000000000000000000 == 0
 > I =      01010101010101010101010101010101 == 1431655765
 > RESULT = 00000010101010101010101010101010 == 44739242
 > I =      10101010101010101010101010101010 == -1431655766
 > RESULT = 11111101010101010101010101010101 == -44739243
 > I =      00000000000000000000000000011111 == 31
 > RESULT = 00000000000000000000000000000000 == 0
 >  characteristics of the result are the same as input
 > kind= 1 shape= 2 2 size= 4
```

### **Standard**

Fortran 2008

### **See Also**

[**shiftl**(3)](#shiftl),
[**shiftr**(3)](#shiftr),
[**ishft**(3)](#ishft),
[**ishftc**(3)](#ishftc)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

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

**shiftl**(3) returns a value corresponding to **i** with all of the
bits shifted left by **shift** places.

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

## shiftr

### **Name**

**shiftr**(3) - \[BIT:SHIFT\] Shift bits right

### **Synopsis**
```fortran
    result = shiftr( i, shift )
```
```fortran
     elemental integer(kind=KIND) function shiftr(i, shift)

      integer(kind=KIND),intent(in) :: i
      integer(kind=SHIFTKIND),intent(in) :: shift
```
### **Characteristics**

where KIND and SHIFTKIND may be any supported _integer_ kind, but
where the kind for **i** dictates the kind of the returned value.

### **Description**

**shiftr**(3) returns a value corresponding to **i** with all of the bits
shifted right by **shift** places. If the absolute value of **shift**
is greater than **bit_size(i)**, the value is undefined. Bits shifted
out from the right end are lost, and bits shifted in from the left end
are set to 0.

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

Fortran 2008

### **See Also**

[**shifta**(3)](#shifta),
[**shiftl**(3)](#shiftl),
[**ishft**(3)](#ishft),
[**ishftc**(3)](#ishftc)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## sign

### **Name**

**sign**(3) - \[NUMERIC\] Sign copying function

### **Synopsis**
```fortran
    result = sign(a, b)
```
```fortran
     elemental type(TYPE(kind=KIND))function sign(a, b)

      type(TYPE(kind=KIND)),intent(in) :: a, b
```
### **Characteristics**

where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.

### **Description**

**sign**(3) returns a value with the magnitude of _a_ but with the
sign of _b_.

For processors that distinguish between positive and negative zeros
_sign()_ may be used to distinguish between _real_ values 0.0 and
-0.0. SIGN (1.0, -0.0) will return  -1.0 when a negative zero is
distinguishable.

### **Options**

  - **a**
    : The value whos magnitude will be returned. Shall be of type
    _integer_ or _real_

  - **b**
    : The value whose sign will be returned. Shall be of the same type
    and kind as **a**

### **Result**

The kind of the return value is the magnitude of _a_ with the sign of
_b_. That is,

  - If _b \>= 0_ then the result is _abs(a)_
  - else if _b < 0_ it is -_abs(a)_.
  - if _b_ is _real_ and the processor distinguishes between _-0.0_
    and _0.0_ then the
    result is _-abs(a)_

### **Examples**

Sample program:
```fortran
program demo_sign
implicit none
   print *,  sign( -12,  1 )
   print *,  sign( -12,  0 )
   print *,  sign( -12, -1 )

   print *,  sign( -12.0, [1.0, 0.0, -1.0] )

   print *,  'can I distinguish 0 from -0? ', &
   &  sign( 1.0, -0.0 ) .ne. sign( 1.0, 0.0 )
end program demo_sign
````
Results:

```text
             12
             12
            -12
      12.00000       12.00000      -12.00000
    can I distinguish 0 from -0?  F
```
### **Standard**

### **See also**

[****(3)](#)

FORTRAN 77

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## sinh

### **Name**

**sinh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function

### **Synopsis**
```fortran
    result = sinh(x)
```
```fortran
     elemental TYPE(kind=KIND) function sinh(x)

      TYPE(kind=KIND) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_
 - **KIND** may be any kind supported by the associated type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

  **sinh**(3) computes the hyperbolic sine of **x**.

  The hyperbolic sine of x is defined mathematically as:
```fortran
     sinh(x) = (exp(x) - exp(-x)) / 2.0
```
  If **x** is of type _complex_ its imaginary part is regarded as a value
  in radians.

### **Options**

- **x**
  : The type shall be _real_ or _complex_.

### **Result**

  The return value has same type and kind as **x**.

### **Examples**

Sample program:
```fortran
program demo_sinh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = - 1.0_real64
real(kind=real64) :: nan, inf
character(len=20) :: line

   print *, sinh(x)
   print *, (exp(x)-exp(-x))/2.0

   ! sinh(3) is elemental and can handle an array
   print *, sinh([x,2.0*x,x/3.0])

   ! a NaN input returns NaN
   line='NAN'
   read(line,*) nan
   print *, sinh(nan)

   ! a Inf input returns Inf
   line='Infinity'
   read(line,*) inf
   print *, sinh(inf)

   ! an overflow returns Inf
   x=huge(0.0d0)
   print *, sinh(x)

end program demo_sinh
```
Results:
```text
  -1.1752011936438014
  -1.1752011936438014
  -1.1752011936438014       -3.6268604078470190      -0.33954055725615012
                       NaN
                  Infinity
                  Infinity
```
### **Standard**

Fortran 95 , for a complex argument Fortran 2008

### **See Also**

[**asinh**(3)](#asinh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_

## sin

### **Name**

**sin**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Sine function

### **Synopsis**
```fortran
    result = sin(x)
```
```fortran
     elemental TYPE(kind=KIND) function sin(x)

      TYPE(kind=KIND) :: x
```
### **Characteristics**

where **TYPE** may be _real_ or _complex_ and **KIND** may be any kind
supported by the associated type.

The returned value will be of the same type and kind as the argument.

### **Description**

**sin**(3) computes the sine of an angle given the size of the angle in
radians.

The sine of an angle in a right-angled triangle is the ratio of the
length of the side opposite the given angle divided by the length of the
hypotenuse.

### **Options**

- **x**
  : The angle in radians to compute the sine of.

### **Result**

- **result**
  : The return value contains the sine of **x**.

### **Examples**

Sample program:

```fortran
program sample_sin
implicit none
real :: x = 0.0
   x = sin(x)
end program sample_sin
```

### **Haversine Formula**

From the article on "Haversine formula" in Wikipedia:

```text
The haversine formula is an equation important in navigation,
giving great-circle distances between two points on a sphere from
their longitudes and latitudes.
```

So to show the great-circle distance between the Nashville International
Airport (BNA) in TN, USA, and the Los Angeles International Airport
(LAX) in CA, USA you would start with their latitude and longitude,
commonly given as

```text
BNA: N 36 degrees 7.2',   W 86 degrees 40.2'
LAX: N 33 degrees 56.4',  W 118 degrees 24.0'
```

which converted to floating-point values in degrees is:

```text
     Latitude Longitude

   - BNA
     36.12, -86.67

   - LAX
     33.94, -118.40
```

And then use the haversine formula to roughly calculate the distance
along the surface of the Earth between the locations:

Sample program:
```fortran
program demo_sin
implicit none
real :: d
    d = haversine(36.12,-86.67, 33.94,-118.40) ! BNA to LAX
    print '(A,F9.4,A)', 'distance: ',d,' km'
contains
function haversine(latA,lonA,latB,lonB) result (dist)
!
! calculate great circle distance in kilometers
! given latitude and longitude in degrees
!
real,intent(in) :: latA,lonA,latB,lonB
real :: a,c,dist,delta_lat,delta_lon,lat1,lat2
real,parameter :: radius = 6371 ! mean earth radius in kilometers,
! recommended by the International Union of Geodesy and Geophysics

! generate constant pi/180
real, parameter :: deg_to_rad = atan(1.0)/45.0
   delta_lat = deg_to_rad*(latB-latA)
   delta_lon = deg_to_rad*(lonB-lonA)
   lat1 = deg_to_rad*(latA)
   lat2 = deg_to_rad*(latB)
   a = (sin(delta_lat/2))**2 + &
          & cos(lat1)*cos(lat2)*(sin(delta_lon/2))**2
   c = 2*asin(sqrt(a))
   dist = radius*c
end function haversine
end program demo_sin
```
Results:
```text
    distance: 2886.4446 km
```
### **Standard**

FORTRAN 77

### **See Also**

[**asin**(3)](#asin),
[**cos**(3)](#cos),
[**tan**(3)](#tan)

### **Resources**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## size

### **Name**

**size**(3) - \[ARRAY INQUIRY\] Determine the size of an array

### **Synopsis**
```fortran
    result = size(array [,dim] [,kind])
```
```fortran
     integer(kind=KIND) function size(array,dim,kind)

      type(TYPE(kind=KIND),intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      integer(kind=**),intent(in),optional :: KIND
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type

-  **array** may be of any type and associated kind.

   If **array** is a pointer it must be associated and allocatable arrays
   must be allocated.

### **Description**

**size**(3) determines the extent of **array** along a specified
dimension **dim**, or the total number of elements in **array** if **dim**
is absent.

### **Options**

- **array**
  : the array to measure the number of elements of.

- **dim**
  : a value shall be
  in the range from 1 to n, where n equals the rank of **array**.

  If not present the total number of elements of the entire array
  are returned.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

  The **kind** must allow for the magnitude returned by **size** or
  results are undefined.

  If **kind** is absent, the return value is of default _integer_ kind.

### **Result**

  If **dim** is not present the total number of elements in the array
  are returned.

  If **dim** is present the number of elements along that dimension
  are returned.

### **Examples**

Sample program:

```fortran
program demo_size
implicit none
integer :: i, j
integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])
   write(*,*) 'SIZE of simple one-dimensional array=', &
   & size([ 11, 22, 33 ])    ! 3

   write(*,*)'body'
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is not "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)

   call interfaced(arr,arr)
   call nointerface(arr)
contains

subroutine interfaced(arr,arr2)
integer,intent(in)  :: arr(:,:)
integer,intent(in)  :: arr2(2,*)
   !
   write(*,*)'interfaced assumed-shape arr2ay'
   !
   ! source argument of shape intrinsic at (1) must not be
   ! an assumed size array
   !!write(*,*)'SHAPE(arr2)       :',shape(arr2)
   ! The upper bound in the last dimension must appear in the reference
   ! to the assumed size array    arr2    at (1)
   !!write(*,*)'SIZE(arr2)        :',size(arr2)
   write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
   !    dim    argument of    size    intrinsic at (1) is not
   !a valid dimension index
   !!write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   ! The upper bound in the last dimension must appear in the
   ! reference to the assumed size array    arr2    at (1)
   !!write(*,*)'UBOUND(arr2)      :',ubound(arr2)
   write(*,*)'LBOUND(arr2,DIM=1):',lbound(arr2,dim=1)
   write(*,*)'UBOUND(arr2,DIM=1):',ubound(arr2,dim=1)
   write(*,*)'LBOUND(arr2,DIM=2):',lbound(arr2,dim=2)
   !    dim    argument of    ubound    intrinsic at (1) is not
   ! a valid dimension index
   !!write(*,*)'UBOUND(arr2,DIM=2):',ubound(arr2,dim=2)
   !
   write(*,*)'interfaced'
   !
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
   !
end subroutine interfaced
!!
! NOTE: If NOINTERFACE(3) had an assumed-shape argument with :
!       for dimensions it could only be properly called with
!       an explicit interface
!!
subroutine nointerface(arr)
integer,intent(in) :: arr(3,*)
   write(*,*)'nointerface'
 ! SHAPE(3) CANNOT BE USED ON AN ASSUMED SIZE ARRAY
 !!write(*,*)'SHAPE(arr)       :',shape(arr)
 !!write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
 ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
 !!write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
 !!write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
 !!write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
end subroutine nointerface
!!
end program demo_size
```

Results:

```text
    SIZE of simple one-dimensional array=           3
    body
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is not "1"
    LBOUND(arr)      :           0          -5
    UBOUND(arr)      :           2           5
    LBOUND(arr,DIM=1):           0
    UBOUND(arr,DIM=1):           2
    LBOUND(arr,DIM=2):          -5
    UBOUND(arr,DIM=2):           5
    interfaced assumed-shape arr2ay
    SIZE(arr2,DIM=1)  :           2
    note lower bound is "1"
    LBOUND(arr2)      :           1           1
    LBOUND(arr2)      :           1           1
    LBOUND(arr2,DIM=1):           1
    UBOUND(arr2,DIM=1):           2
    LBOUND(arr2,DIM=2):           1
    interfaced
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr)      :           1           1
    UBOUND(arr)      :           3          11
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
    UBOUND(arr,DIM=2):          11
    nointerface
    SIZE(arr,DIM=1)  :           3
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
```

### **Standard**

Fortran 95 , with **kind** argument - Fortran 2003

### **See Also**

#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contigious) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry:

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## spacing

### **Name**

**spacing**(3) - \[MODEL_COMPONENTS\] Smallest distance between two numbers of a given type

### **Synopsis**
```fortran
    result = spacing(x)
```
```fortran
     elemental real(kind=KIND) function spacing(x)

      real(kind=KIND), intent(in) :: x
```
### **Characteristics**

  The result is of the same type as the input argument **x**.

### **Description**

  **spacing**(3) determines the distance between the argument **x**
  and the nearest adjacent number of the same type.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The result is of the same type as the input argument **x**.

### **Examples**

Sample program:

```fortran
program demo_spacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)

   write(*,*) spacing(1.0_sgl)
   write(*,*) nearest(1.0_sgl,+1.0),nearest(1.0_sgl,+1.0)-1.0

   write(*,*) spacing(1.0_dbl)
end program demo_spacing
```
Results:

Typical values ...

```text
     1.1920929E-07
      1.000000      1.1920929E-07
     0.9999999     -5.9604645E-08
     2.220446049250313E-016
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_

## spread

### **Name**

**spread**(3) - \[ARRAY CONSTRUCTION\] Add a dimension and replicate data

### **Synopsis**
```fortran
    result = spread(source, dim, ncopies)
```
```fortran
     TYPE(kind=KIND) function spread(source, dim, ncopies)

      TYPE(kind=KIND)             :: source(..)
      integer(kind=**),intent(in) :: dim
      integer(kind=**),intent(in) :: ncopies
```
### **Characteristics**

- **source** is a scalar or array of any type.
- **dim** is an _integer_ scalar
- **ncopies** is an integer scalar

### **Description**

**spread**(3) replicates a **source** array along a specified dimension
**dim**. The copy is repeated **ncopies** times.

So to add additional rows to a matrix **dim=1** would be used, but to
add additional rows **dim=2** would be used, for example.

If **source** is scalar, the size of the resulting vector is **ncopies**
and each element of the result has a value equal to **source**.

### **Options**

- **source**
  : a scalar or array of any type and a rank less than fifteen.

- **dim**

  : The additional dimension  value in the range from
  **1** to **n+1**, where **n** equals the rank of **source**.

- **ncopies**
  : the number of copies of the original data to generate

### **Result**

The result is an array of the same type as **source** and has rank **n+1**
where **n** equals the rank of **source**.

### **Examples**

Sample program:

```fortran
program demo_spread
implicit none

integer a1(4,3), a2(3,4), v(4), s, i

   write(*,'(a)' ) &
   'TEST SPREAD(3)                                      ', &
   '  SPREAD(3) is a FORTRAN90 function which replicates', &
   '  an array by adding a dimension.                   ', &
   ' '

   s = 99
   call printi('suppose we have a scalar S',s)

   write(*,*) 'to add a new dimension (1) of extent 4 call'
   call printi('spread( s, dim=1, ncopis=4 )',spread ( s, 1, 4 ))

   v = [ 1, 2, 3, 4 ]
   call printi(' first we will set V to',v)

   write(*,'(a)')' and then do "spread ( v, dim=2, ncopies=3 )"'
   a1 = spread ( v, dim=2, ncopies=3 )
   call printi('this adds a new dimension (2) of extent 3',a1)

   a2 = spread ( v, 1, 3 )
   call printi(' spread(v,1,3) adds a new dimension (1) of extent 3',a2)
   ! add more
   a2 = spread ( v, 1, 3 )
   call printi(' spread(v,1,3) adds a new dimension (1) of extent 3',a2)

contains
! CONVENIENCE ROUTINE; NOT DIRECTLY CONNECTED TO SPREAD(3)
subroutine printi(title,a)
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,&
 & stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
implicit none

!@(#) print small 2d integer scalar, vector, matrix in row-column format

character(len=*),parameter   :: all='(" ",*(g0,1x))'
character(len=*),intent(in)  :: title
character(len=20)            :: row
integer,intent(in)           :: a(..)
integer                      :: i

   write(*,all,advance='no')trim(title)
   ! select rank of input
   select rank(a)
   rank (0); write(*,'(a)')' (a scalar)'
      write(*,'(" > [ ",i0," ]")')a
   rank (1); write(*,'(a)')' (a vector)'
      ! find how many characters to use for integers
      write(row,'(i0)')ceiling(log10(real(maxval(abs(a)))))+2
      ! use this format to write a row
      row='(" > [",*(i'//trim(row)//':,","))'
      do i=1,size(a)
         write(*,fmt=row,advance='no')a(i)
         write(*,'(" ]")')
      enddo
   rank (2); write(*,'(a)')' (a matrix) '
      ! find how many characters to use for integers
      write(row,'(i0)')ceiling(log10(real(maxval(abs(a)))))+2
      ! use this format to write a row
      row='(" > [",*(i'//trim(row)//':,","))'
      do i=1,size(a,dim=1)
         write(*,fmt=row,advance='no')a(i,:)
         write(*,'(" ]")')
      enddo
   rank default
      write(stderr,*)'*printi* did not expect rank=', rank(a), &
       & 'shape=', shape(a),'size=',size(a)
      stop '*printi* unexpected rank'
   end select
   write(*,all) '>shape=',shape(a),',rank=',rank(a),',size=',size(a)
   write(*,*)

end subroutine printi

end program demo_spread
```
Results:
```text
   TEST SPREAD(3)
     SPREAD(3) is a FORTRAN90 function which replicates
     an array by adding a dimension.

    suppose we have a scalar S  (a scalar)
    > [ 99 ]
    >shape= ,rank= 0 ,size= 1

    to add a new dimension (1) of extent 4 call
    spread( s, dim=1, ncopies=4 )  (a vector)
    > [  99 ]
    > [  99 ]
    > [  99 ]
    > [  99 ]
    >shape= 4 ,rank= 1 ,size= 4

     first we will set V to  (a vector)
    > [  1 ]
    > [  2 ]
    > [  3 ]
    > [  4 ]
    >shape= 4 ,rank= 1 ,size= 4

    and then do "spread ( v, dim=2, ncopies=3 )"
    this adds a new dimension (2) of extent 3  (a matrix)
    > [  1,  1,  1 ]
    > [  2,  2,  2 ]
    > [  3,  3,  3 ]
    > [  4,  4,  4 ]
    >shape= 4 3 ,rank= 2 ,size= 12

     spread(v,dim=1,ncopies=3) adds a new dimension (1) (a matrix)
    > [  1,  2,  3,  4 ]
    > [  1,  2,  3,  4 ]
    > [  1,  2,  3,  4 ]
    >shape= 3 4 ,rank= 2 ,size= 12
```
### **Standard**

Fortran 95

### **See Also**

[**merge**(3)](#merge),
[**pack**(3)](#pack),
[**unpack**(3)](#unpack)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
<!--
when adding dimension 3,4,5, ... why is 15 not allowed if 16 is allowed?

need an illustration of what happens with higher dimension array
-->
#

## sqrt

### **Name**

**sqrt**(3) - \[MATHEMATICS\] Square-root function

### **Synopsis**
```fortran
    result = sqrt(x)
```
```fortran
     elemental TYPE(kind=KIND) function sqrt(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **TYPE** may be _real_ or _complex_.
 - **KIND** may be any kind valid for the declared type.

### **Description**

  **sqrt**(3) computes the principal square root of **x**.

  In mathematics, a square root of a number **x** is a number **y**
  such that **y\*y = x**.

  The number whose square root is being considered is known as the
  _radicand_.

  Every nonnegative number _x_ has two square roots of the same unique
  magnitude, one positive and one negative. The nonnegative square root
  is called the principal square root.

  The principal square root of 9 is 3, for example, even though (-3)\*(-3)
  is also 9.

  A _real_ radicand must be positive.

  Square roots of negative numbers are a special case of complex numbers,
  where the components of the _radicand_ need not be positive in order
  to have a valid square root.

### **Options**

- **x**
  : If **x** is _real_ its value must be greater than or equal to zero.
  The type shall be _real_ or _complex_.

### **Result**

  The return value is of type _real_ or _complex_. The kind type parameter
  is the same as **x**.

  A result of type complex is the principal value with the real part
  greater than or equal to zero. When the real part of the result is zero,
  the imaginary part has the same sign as the imaginary part of X.

### **Examples**

Sample program:

```fortran
program demo_sqrt
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x, x2
complex :: z, z2

   x = 2.0_real64
   z = (1.0, 2.0)
   write(*,*)x,z

   x2 = sqrt(x)
   z2 = sqrt(z)
   write(*,*)x2,z2

   x2 = x**0.5
   z2 = z**0.5
   write(*,*)x2,z2

end program demo_sqrt
```
Results:

```text
  2.0000000000000000    (1.00000000,2.00000000)
  1.4142135623730951    (1.27201962,0.786151350)
  1.4142135623730951    (1.27201962,0.786151350)
```
### **Standard**

FORTRAN 77

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## storage_size

### **Name**

**storage_size**(3) - \[BIT:INQUIRY\] Storage size in bits

### **Synopsis**
```fortran
    result = storage_size(a [,kind] )
```
```fortran
     integer(kind=KIND) storage_size(a,kind)

      type(TYPE(kind=**)) :: a
      integer,intent(in),optional :: kind
```
### **Characteristics**
  - a kind designated as ** may be any supported kind value for the type

  - **a** may be of any type and kind. If it is polymorphic it shall not
    be an undefined pointer. If it is unlimited polymorphic or has any
    deferred type parameters, it shall not be an unallocated allocatable
    variable or a disassociated or undefined pointer.

  - The kind type parameter of the returned value is that specified by
    the value of **kind**; otherwise, the kind type parameter is that of
    default integer type.

### **Description**

**storage_size**(3) returns the storage size of argument **a** in bits.

### **Options**

- **a**
  : The entity to determine the storage size of

- **kind**
  : a scalar integer constant expression that defines the kind of the
  output value.

### **Result**

  The result value is the size expressed in bits for an element of an
  array that has the dynamic type and type parameters of **a**.

  If the type and type parameters are such that storage association
  applies, the result is consistent with the named constants
  defined in the intrinsic module ISO_FORTRAN_ENV.

   NOTE1

    An array element might take "type" more bits to store than an isolated
    scalar, since any hardware-imposed alignment requirements for
    array elements might not apply to a simple scalar variable.

   NOTE2

    This is intended to be the size in memory that an object takes when
    it is stored; this might differ from the size it takes during
    expression handling (which might be the native register size) or
    when stored in a file. If an object is never stored in memory
    but only in a register, this function nonetheless returns the size
    it would take if it were stored in memory.

### **Examples**

Sample program
```fortran
program demo_storage_size
implicit none
   write(*,*)'size of integer       ',storage_size(0)
   write(*,*)'size of real          ',storage_size(0.0)
   write(*,*)'size of logical       ',storage_size(.true.)
   write(*,*)'size of complex       ',storage_size((0.0,0.0))
   write(*,*)'size of integer array ',storage_size([0,1,2,3,4,5,6,7,8,9])
end program demo_storage_size
```
Results:
```text
    size of integer                 32
    size of real                    32
    size of logical                 32
    size of complex                 64
    size of integer array           32
```
### **Standard**

Fortran 2008

### **See Also**

[**c_sizeof**(3)](#c_sizeof)

 _fortran-lang intrinsic descriptions_

## sum

### **Name**

**sum**(3) - \[ARRAY REDUCTION\] Sum the elements of an array

### **Synopsis**
```fortran
   result = sum(array ,[mask])
```
```fortran
     NUMERIC function sum(array, mask)

      NUMERIC,intent(in) :: array(..)
      logical(kind=**),intent(in),optional :: mask(..)
```
  or
```fortran
   result = sum(array [,dim] [,mask])
```
```fortran
     NUMERIC function sum(array, dim, mask)

      NUMERIC,intent(in) :: array(..)
      integer(kind=**),intent(in),optional :: dim
      logical(kind=**),intent(in),optional :: mask(..)
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type
  - **NUMERIC** is any numeric type and kind.

### **Description**

  **sum**(3) adds the elements of ARRAY along dimension DIM if the
  corresponding element in MASK is TRUE.

### **Options**

- **array**
  : Shall be an array of type _integer_, _real_ or _complex_.

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from 1 to n, where n equals the rank of ARRAY.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as ARRAY.

### **Result**

The result is of the same type as ARRAY.

If **dim** is absent, a scalar with the sum of all elements in ARRAY
is returned. Otherwise, an array of rank n-1, where n equals the rank of
ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped
is returned.

### **Examples**

Sample program:

```fortran
program simple_sum
implicit none
integer :: x(5) = [ 1, 2, 3, 4, 5 ]
   print *, sum(x)                        ! all elements, sum = 15
   print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9
end program simple_sum
```

Demonstrate Fortran 90 SUM function with MASK option

```fortran
program demo_sum
! John Mahaffy  2/16/96
implicit none
integer nd, ndh, nduh, j
parameter (nd=10,ndh=nd/2, nduh=nd-ndh)
real csum, cpsum, cbpsum
real, dimension(nd):: c=[(j, j=-1,nd-2)], b
data b/ndh*-1.0, nduh*2.0/
   csum= sum(c(1:nd))
   cpsum= sum (c(1:nd), mask=c.gt.0)
   cbpsum= sum(c(1:nd), mask=b.gt.0.0)
   print *, 'Sum of all elements in c = ', csum
   print *, 'Sum of Positive elements in c = ', cpsum
   print *, 'Sum of elements in c when corresponding elements in b>0', &
   & ' =', cbpsum
end program demo_sum
```

Results:

```text
 Sum of all elements in c =    35.0000000
 Sum of Positive elements in c =    36.0000000
 Sum of elements in c when corresponding elements in b>0 =   30.0000000
```

### **Standard**

Fortran 95

### **See Also**

intrinsics

 _fortran-lang intrinsic descriptions_

## system_clock

### **Name**

**system_clock**(3) - \[SYSTEM:TIME\] Return numeric data from a real-time clock.

### **Synopsis**
```fortran
    call system_clock([count] [,count_rate] [,count_max] )
```
```fortran
     subroutine system_clock(count, count_rate, count_max)

      integer,intent(out),optional  :: count
      type(TYPE(kind=KIND),intent(out),optional  :: count_rate
      integer,intent(out),optional  :: count_max
```
### **Characteristics**

where TYPE may be _real_ or _integer_.

### **Description**

**system_clock**(3) lets you measure durations of time with the precision of
the smallest time increment generally available on a system by returning
processor-dependent values based on the current value of the processor
clock. The **clock** value is incremented by one for each clock count until
the value **count_max** is reached and is then reset to zero at the next
count. **clock** therefore is a modulo value that lies in the range **0 to
count_max**. **count_rate** and **count_max** are assumed constant (even though
CPU rates can vary on a single platform).

**count_rate** is system dependent and can vary depending on the kind of
the arguments.

If there is no clock, or querying the clock fails, **count** is set to
**-huge(count)**, and **count_rate** and **count_max** are set to zero.

**system_clock** is typically used to measure short time intervals (system
clocks may be 24-hour clocks or measure processor clock ticks since
boot, for example). It is most often used for measuring or tracking the
time spent in code blocks in lieu of using profiling tools.

### **Options**

- **count**
  : (optional) shall be an _integer_ scalar. It is assigned a
  processor-dependent value based on the current value of the
  processor clock, or **-huge(count)** if there is no clock. The
  processor-dependent value is incremented by one for each clock count
  until the value **count_max** is reached and is reset to zero at the
  next count. It lies in the range **0** to **count_max** if there is a
  clock.

- **count_rate**
  : (optional) shall be an _integer_ or _real_ scalar. It is assigned a
  processor-dependent approximation to the number of processor clock
  counts per second, or zero if there is no clock.

- **count_max**
  : (optional) shall be an _integer_ scalar. It is assigned the maximum
  value that **COUNT** can have, or zero if there is no clock.

### **Examples**

Sample program:

```fortran
program demo_system_clock
implicit none
integer, parameter :: wp = kind(1.0d0)
integer :: count, count_rate, count_max
integer :: start, finish
real    :: time_read

   call system_clock(count, count_rate, count_max)
   write(*,*) count, count_rate, count_max

   call system_clock(start, count_rate)
   ! <<<< code to time
   call system_clock(finish)
   time_read=(finish-start)/real(count_rate,wp)
   write(*,'(a30,1x,f7.4,1x,a)') 'time * : ', time_read, ' seconds'

end program demo_system_clock
```

If the processor clock is a 24-hour clock that registers time at
approximately 18.20648193 ticks per second, at 11:30 A.M. the reference

```fortran
      call system_clock (count = c, count_rate = r, count_max = m)
```

defines

```text
      C = (11*3600+30*60)*18.20648193 = 753748,
      R = 18.20648193, and
      M = 24*3600*18.20648193-1 = 1573039.
```

### **Standard**

Fortran 95

### **See Also**

[**date_and_time**(3)](#date_and_time),
[**cpu_time**(3)](#cpu_time)

 _fortran-lang intrinsic descriptions_

## tanh

### **Name**

**tanh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic tangent function

### **Synopsis**
```fortran
    result = tanh(x)
```
```fortran
     elemental TYPE(kind=KIND) function tanh(x)

      TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

where **TYPE** may be _real_ or _complex_ and **KIND** may be any kind
supported by the associated type. The returned value will be of the same
type and kind as the argument.

### **Description**

**tanh**(3) computes the hyperbolic tangent of **x**.

### **Options**

- **x**
  : The value to compute the Hyperbolic tangent of

### **Result**

The return value has same type and kind as **x**. If **x** is complex, the
imaginary part of the result is in radians. If **x** is _real_, the return
value lies in the range

```
      -1 <= tanh(x) <= 1.
```
### **Examples**

Sample program:
```fortran
program demo_tanh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 2.1_real64
   write(*,*)x, tanh(x)
end program demo_tanh
```
Results:
```text
      2.1000000000000001       0.97045193661345386
```
### **Standard**

FORTRAN 77 , for a complex argument Fortran 2008

### **See Also**

[**atanh**(3)](#atanh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_

## tan

### **Name**

**tan**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Tangent function

### **Synopsis**
```fortran
result = tan(x)
```
```fortran
 elemental TYPE(kind=KIND) function tan(x)

  TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

  - **TYPE** may be _real_ or _complex_
  - **KIND** may be any kind supported by the associated type.
  - The returned value will be of the same type and kind as the argument
    **x**.

### **Description**

**tan**(3) computes the tangent of **x**.

### **Options**

- **x**
  : The angle in radians to compute the tangent of for _real_ input.
    If **x** is of type _complex_, its real part is regarded as a value
    in radians.

### **Result**

  The return value is the tangent of the value **x**.

### **Examples**

Sample program:
```fortran
program demo_tan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 0.165_real64
     write(*,*)x, tan(x)
end program demo_tan
```
Results:
```text
     0.16500000000000001       0.16651386310913616
```
### **Standard**

FORTRAN 77 . For a complex argument, Fortran 2008 .

### **See Also**

[**atan**(3)](#atan),
[**atan2**(3)](#atan2),
[**cos**(3)](#cos),
[**sin**(3)](#sin)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## this_image

### **Name**

**this_image**(3) - \[COLLECTIVE\] Cosubscript index of this image

### **Synopsis**

```fortran
 result = this_image()
```
or
```fortran
 result = this_image(distance)
```
or
```fortran
  result = this_image(coarray, dim)
```
```fortran
   integer function this_image( distance ,coarray, dim )

    type(TYPE(kind=**),optional :: coarray[*]
    integer,intent(in),optional :: distance
    integer,intent(in),optional :: dim
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type
 - **distance** (not permitted together with **coarray**).
 - **coarray** can be of any type. If **dim** is present it is required.
 - if **dim**  if present, coarray is required.

### **Description**

**this_image**(3) returns the cosubscript for this image.

### **Options**

- **distance**
  : Nonnegative scalar integer (not permitted together with **coarray**).

- **coarray**
  : if **dim** present, required).

- **dim**
  : If present, **dim** shall be between one and the corank of **coarray**.

### **Result**

Default integer. If **coarray** is not present, it is scalar; if **distance** is
not present or has value **0**, its value is the image index on the invoking
image for the current team, for values smaller or equal distance to the
initial team, it returns the image index on the ancestor team which has
a distance of **distance** from the invoking team. If **distance** is larger
than the distance to the initial team, the image index of the initial
team is returned. Otherwise when the **coarray** is present, if **dim** is not
present, a rank-1 array with corank elements is returned, containing the
cosubscripts for **coarray** specifying the invoking image. If **dim** is
present, a scalar is returned, with the value of the **dim** element of
**this_image(coarray)**.

### **Examples**

Sample program:
```fortran
program demo_this_image
implicit none
integer :: value[*]
integer :: i
   value = this_image()
   sync all
   if (this_image() == 1) then
      do i = 1, num_images()
         write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
      end do
   endif
end program demo_this_image
```
Results:

```text
   value[1] is 1
```
### **Standard**

Fortran 2008. With DISTANCE argument, TS 18508

### **See Also**

[**num\_images**(3)](#num_images),
[**image\_index**(3)](#image_index)

 _fortran-lang intrinsic descriptions_
```

## tiny

### **Name**

**tiny**(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind

### **Synopsis**
```fortran
    result = tiny(x)
```
```fortran
     real(kind=KIND) function tiny(x)

      real(kind=KIND) :: x
```
### **Characteristics**

where KIND may be any kind supported by type _real_

### **Description**

**tiny**(3) returns the smallest positive (non zero) number of the type
and kind of **x**.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The smallest positive value for the _real_ type of the specified kind.

The return value is of the same type and kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_tiny
implicit none
   print *, 'default real is from', tiny(0.0), 'to',huge(0.0)
   print *, 'doubleprecision is from ', tiny(0.0d0), 'to',huge(0.0d0)
end program demo_tiny
```

Results:

```text
 default real is from 1.17549435E-38 to 3.40282347E+38
 doubleprecision is from 2.2250738585072014E-308 to
 1.7976931348623157E+308
```

### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing)

 _fortran-lang intrinsic descriptions_

## trailz

### **Name**

**trailz**(3) - \[BIT:COUNT\] Number of trailing zero bits of an integer

### **Synopsis**
```fortran
    result = trailz(i)
```
```fortran
     elemental integer function trailz(i)

      integer(kind=KIND),intent(in) :: i
```
### **Characteristics**

### **Description**

**trailz**(3) returns the number of trailing zero bits of an _integer_ value.

### **Options**

- **i**
  : Shall be of type _integer_.

### **Result**

The type of the return value is the default _integer_. If all the bits of
I are zero, the result value is **bit_size(i)**.

### **Examples**

Sample program:

```fortran
program demo_trailz
use, intrinsic :: iso_fortran_env, only : &
 & integer_kinds, int8, int16, int32, int64
implicit none
integer(kind=int64) :: i, value
   write(*,*)'Default integer:'
   write(*,*)'bit_size=',bit_size(0)
   write(*,'(1x,i3,1x,i3,1x,b0)')-1,trailz(1),-1
   write(*,'(1x,i3,1x,i3,1x,b0)')0,trailz(0),0
   write(*,'(1x,i3,1x,i3,1x,b0)')1,trailz(1),1
   write(*,'(" huge(0)=",i0,1x,i0,1x,b0)') &
   & huge(0),trailz(huge(0)),huge(0)
   write(*,*)
   write(*,*)'integer(kind=int64):'

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i19,1x,i3)')value,trailz(value)
   enddo
   value=huge(i)
   write(*,'(1x,i19,1x,i3,"(huge(0_int64))")')value,trailz(value)

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i3,2x,b64.64)')i,value
   enddo
   value=huge(i)
   write(*,'(1x,a,1x,b64.64)') "huge",value

end program demo_trailz
```

Results:

```
 Default integer:
 bit_size=          32
  -1   0 11111111111111111111111111111111
   0  32 0
   1   0 1
 huge(0)=2147483647 0 1111111111111111111111111111111

 integer(kind=int64):
                   0  64
                  16   4
                 512   9
               16384  14
              524288  19
            16777216  24
           536870912  29
         17179869184  34
        549755813888  39
      17592186044416  44
     562949953421312  49
   18014398509481984  54
  576460752303423488  59
 9223372036854775807   0(huge(0_int64))
  -1  0000000000000000000000000000000000000000000000000000000000000000
   4  0000000000000000000000000000000000000000000000000000000000010000
   9  0000000000000000000000000000000000000000000000000000001000000000
  14  0000000000000000000000000000000000000000000000000100000000000000
  19  0000000000000000000000000000000000000000000010000000000000000000
  24  0000000000000000000000000000000000000001000000000000000000000000
  29  0000000000000000000000000000000000100000000000000000000000000000
  34  0000000000000000000000000000010000000000000000000000000000000000
  39  0000000000000000000000001000000000000000000000000000000000000000
  44  0000000000000000000100000000000000000000000000000000000000000000
  49  0000000000000010000000000000000000000000000000000000000000000000
  54  0000000001000000000000000000000000000000000000000000000000000000
  59  0000100000000000000000000000000000000000000000000000000000000000
 huge 0111111111111111111111111111111111111111111111111111111111111111
```

### **Standard**

Fortran 2008

### **See Also**

[**bit_size**(3)](#bit_size),
[**popcnt**(3)](#popcnt),
[**poppar**(3)](#poppar),
[**leadz**(3)](#leadz)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## transfer

### **Name**

**transfer**(3) - \[TYPE:MOLD\] Transfer bit patterns

### **Synopsis**
```fortran
    result = transfer(source, mold [,size] )
```
```fortran
     type(TYPE(kind=KIND)) function transfer(source,mold,size)

      type(TYPE(kind=KIND)),intent(in) :: source(..)
      type(TYPE(kind=KIND)),intent(in) :: mold(..)
      integer,intent(in),intent(in),optional :: size
```
### **Characteristics**

- **source** shall be a scalar or an array of any type.
- **mold** shall be a scalar or an array of any type.
- **size** shall be a scalar of type _integer_.
- **result** has the same type as **mold**

### **Description**

**transfer**(3) copies the bitwise representation of **source** in memory
into a variable or array of the same type and type parameters as **mold**.

This is approximately equivalent to the C concept of "casting" one type
to another.

### **Options**

- **source**
  : Holds the bit pattern to be copied

- **mold**
  : the type of **mold** is used to define the type of the returned
  value. In addition, if it is an array the returned value is a
  one-dimensional array. If it is a scalar the returned value is a scalar.

- **size**
  : If **size** is present, the result is a one-dimensional array of
    length **size**.

If **size** is absent but **mold** is an array (of any size or
shape), the result is a one-dimensional array of the minimum length
needed to contain the entirety of the bitwise representation of **source**.

If **size** is absent and **mold** is a scalar, the result is a scalar.

### **Result**

The result has the bit level representation of **source**.

If the bitwise representation of the result is longer than that of
**source**, then the leading bits of the result correspond to those of
**source** but any trailing bits are filled arbitrarily.

When the resulting bit representation does not correspond to a valid
representation of a variable of the same type as **mold**, the results are
undefined, and subsequent operations on the result cannot be guaranteed to
produce sensible behavior. For example, it is possible to create _logical_
variables for which **var** and .not. var both appear to be true.

### **Examples**

Sample program:

```fortran
program demo_transfer
use,intrinsic :: iso_fortran_env, only : int32, real32
integer(kind=int32) :: i = 2143289344
real(kind=real32)   :: x
character(len=10)   :: string
character(len=1)    :: chars(10)
   x=transfer(i, 1.0)    ! prints "nan" on i686
   ! the bit patterns are the same
   write(*,'(b0,1x,g0)')x,x ! create a NaN
   write(*,'(b0,1x,g0)')i,i

   ! a string to an array of characters
   string='abcdefghij'
   chars=transfer(string,chars)
   write(*,'(*("[",a,"]":,1x))')string
   write(*,'(*("[",a,"]":,1x))')chars
end program demo_transfer
```

Results:

```text
   1111111110000000000000000000000 NaN
   1111111110000000000000000000000 2143289344
   [abcdefghij]
   [a] [b] [c] [d] [e] [f] [g] [h] [i] [j]
```

### **Comments**

_Joe Krahn_: Fortran uses **molding** rather than **casting**.

Casting, as in C, is an in-place reinterpretation. A cast is a device
that is built around an object to change its shape.

Fortran TRANSFER reinterprets data out-of-place. It can be considered
**molding** rather than casting. A **mold** is a device that
confers a shape onto an object placed into it.

The advantage of molding is that data is always valid in the context
of the variable that holds it. For many cases, a decent compiler should
optimize TRANSFER into a simple assignment.

There are disadvantages of this approach. It is problematic to define a
union of data types because you must know the largest data object, which
can vary by compiler or compile options. In many cases, an EQUIVALENCE
would be far more effective, but Fortran Standards committees seem
oblivious to the benefits of EQUIVALENCEs when used sparingly.

### **Standard**

Fortran 90

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_

## transpose

### **Name**

**transpose**(3) - \[ARRAY MANIPULATION\] Transpose an array of rank two

### **Synopsis**
```fortran
    result = transpose(matrix)
```
```fortran
     type(TYPE(kind=KIND) transpose(matrix)

      type(TYPE(kind=KIND),intent(in) :: matrix(:,:)
```
### **Characteristics**

  - **matrix** can be of any type but must have a rank of two.
  - The result will be the same type and kind as **matrix** and the
    reversed shape of the input array.

### **Description**

  **transpose**(3) transposes an array of rank two.

  An array is transposed by interchanging the rows and columns of the
  given matrix. That is, element (i,j) of the result has the value of
  element (j,i) of the input for all (i,j).

### **Options**

- **matrix**
  : The array to transpose

### **Result**

The transpose of the input array. The result has the same type as
**matrix**, and has shape \[ m, n \] if **matrix** has shape \[ n, m \].

### **Examples**

Sample program:
```fortran
program demo_transpose
implicit none
integer,save :: xx(3,5)= reshape([&
    1,  2,  3,  4,  5,    &
   10, 20, 30, 40, 50,    &
   11, 22, 33, 44, -1055  &
 ],shape(xx),order=[2,1])

call print_matrix_int('xx array:',xx)
call print_matrix_int('xx array transposed:',transpose(xx))

contains

subroutine print_matrix_int(title,arr)
! print small 2d integer arrays in row-column format
implicit none
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest
   write(*,*)trim(title)  ! print title
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo
end subroutine print_matrix_int

end program demo_transpose
```
Results:
```
    xx array:
    > [     1,     2,     3,     4,     5 ]
    > [    10,    20,    30,    40,    50 ]
    > [    11,    22,    33,    44, -1055 ]
    xx array transposed:
    > [     1,    10,    11 ]
    > [     2,    20,    22 ]
    > [     3,    30,    33 ]
    > [     4,    40,    44 ]
    > [     5,    50, -1055 ]
```
### **Standard**

Fortran 95

### **See also**

- [**merge**(3)](#merge) - Merge variables
- [**pack**(3)](#pack) - Pack an array into an array of rank one
- [**spread**(3)](#spread) - Add a dimension and replicate data
- [**unpack**(3)](#unpack) - Scatter the elements of a vector

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## trim

### **Name**

**trim**(3) - \[CHARACTER:WHITESPACE\] Remove trailing blank characters of a string

### **Synopsis**
```fortran
    result = trim(string)
```
```fortran
     character(len=:,kind=KIND) function trim(string)

      character(len=*,kind=KIND),intent(in) :: string
```
### **Characteristics**

  **KIND** can be any kind supported for the _character_ type.
  The result has the same kind as the input argument **string**.

### **Description**

  **trim**(3) removes trailing blank characters from a string.

### **Options**

- **string**
  : A string to trim

### **Result**

  The result is the same as **string** except trailing blanks are removed.

  If **string** is composed entirely of blanks or has zero length,
  the result has zero length.

### **Examples**

Sample program:

```fortran
program demo_trim
implicit none
character(len=:), allocatable :: str, strs(:)
character(len=*),parameter :: brackets='( *("[",a,"]":,1x) )'
integer :: i

   str='   trailing    '
   print brackets, str,trim(str) ! trims it

   str='   leading'
   print brackets, str,trim(str) ! no effect

   str='            '
   print brackets, str,trim(str) ! becomes zero length
   print *,  len(str), len(trim('               '))

  ! array elements are all the same length, so you often
  ! want to print them
   strs=[character(len=10) :: "Z"," a b c","ABC",""]

   write(*,*)'untrimmed:'
   ! everything prints as ten characters; nice for neat columns
   print brackets, (strs(i), i=1,size(strs))
   print brackets, (strs(i), i=size(strs),1,-1)
   write(*,*)'trimmed:'
   ! everything prints trimmed
   print brackets, (trim(strs(i)), i=1,size(strs))
   print brackets, (trim(strs(i)), i=size(strs),1,-1)

end program demo_trim
```
Results:
```text
    > [   trailing    ] [   trailing]
    > [   leading] [   leading]
    > [            ] []
    >           12           0
    >  untrimmed:
    > [Z         ] [ a b c    ] [ABC       ] [          ]
    > [          ] [ABC       ] [ a b c    ] [Z         ]
    >  trimmed:
    > [Z] [ a b c] [ABC] []
    > [] [ABC] [ a b c] [Z]
```
### **Standard**

Fortran 95

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),
  [**scan**(3)](#scan),
  [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## ubound

### **Name**

**ubound**(3) - \[ARRAY INQUIRY\] Upper dimension bounds of an array

### **Synopsis**
```fortran
    result = ubound(array [,dim] [,kind] )
```
```fortran
     elemental TYPE(kind=KIND) function ubound(array,dim,kind)

      TYPE(kind=KIND),intent(in)  :: array
      integer(kind=**),intent(in),optional :: dim
      integer(kind=**),intent(in),optional :: kind
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **array** shall be an array, of any type.
- **dim** shall be a scalar _integer_.
- **kind** an _integer_ initialization expression indicating the kind
  parameter of the result.
- The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default integer kind.

### **Description**

**ubound**(3) returns the upper bounds of an array, or a single upper
bound along the **dim** dimension.

### **Options**

- **array**
  : The array to determine the upper bounds of

- **dim**
  : a specific rank to determine the bounds of
  If **dim** is absent, the result is an array of the upper bounds of
  **array**.

- **kind**
  : indicates the kind parameter of the result. If absent, an _integer_
  of the default kind is returned.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default integer kind.

If **dim** is absent, the result is an array of the upper bounds of
**array**.

If **dim** is present, the result is a scalar corresponding to the upper
bound of the array along that dimension.

If **array** is an expression rather than a whole array or array
structure component, or if it has a zero extent along the relevant
dimension, the upper bound is taken to be the number of elements along
the relevant dimension.

### **Examples**

Note this function should not be used on assumed-size arrays or in any
function without an explicit interface. Errors can occur if there is no
interface defined.

Sample program

```fortran
! program demo_ubound
module m2_bounds
implicit none

contains

subroutine msub(arr)
!!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
integer,intent(in) :: arr(:)
   write(*,*)'MSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine msub

end module m2_bounds

use m2_bounds, only : msub
implicit none
interface
   subroutine esub(arr)
   integer,intent(in) :: arr(:)
   end subroutine esub
end interface
integer :: arr(-10:10)
   write(*,*)'MAIN: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
   call csub()
   call msub(arr)
   call esub(arr)
contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub

end

subroutine esub(arr)
implicit none
integer,intent(in) :: arr(:)
   ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
   ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
   write(*,*)'ESUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine esub
!end program demo_ubound
```

Results:

```text
  MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
  CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
  MSUB: LOWER=           1 UPPER=          21 SIZE=          21
  ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```

### **Standard**

Fortran 95 , with KIND argument Fortran 2003

### **See Also**

#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

[**co_ubound**(3)](#co_ubound),
[__co\_lbound__(3)(co_lbound)]

#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contigious) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry:

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.
- [**lbound**(3)](#lbound),

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

## unpack

### **Name**

**unpack**(3) - \[ARRAY CONSTRUCTION\] Scatter the elements of a vector
into an array using a mask

### **Synopsis**
```fortran
    result = unpack(vector, mask, field)
```
```fortran
     type(TYPE(kind=KIND)) unpack(vector, mask, field)

      type(TYPE(kind=KIND)),intent(in) :: vector(:)
      logical,intent(in)               :: mask(..)
      type(TYPE(kind=KIND)),intent(in) :: field(..)
```
### **Characteristics**

The result is an array of the same type and type parameters as **vector**
and the same shape as **mask**.

### **Description**

**unpack**(3) scatters the elements of **vector** into a copy of an
array **field** of any rank using _.true._ values from **mask** in array
element order to specify placement of the **vector** values.

So a copy of **field** is generated with select elements replaced with
values from **vector**. This allows for complex replacement patterns
that would be difficult when using array syntax or multiple assignment
statements, particularly when the replacements are conditional.

### **Options**

- **vector**
  : New values to place into specified locations in **field**. Shall
  be an array of any type and rank one. It shall have at least as many
  elements as **mask** has _.true._ values.

- **mask**
  : Shall be an array of type _logical_ that specifies which values
  in **field** are to be replaced with values from **vector**.

- **field**
  : The original data to be edited. Shall be of the same type and type
  parameters as **vector** and shall be conformable with **mask**.

### **Result**

The element of the result that corresponds to the ith true element
of MASK, in array element order, has the value VECTOR (i) for i =
1, 2, . . ., t, where t is the number of true values in MASK. Each
other element has a value equal to FIELD if FIELD is scalar or to the
corresponding element of FIELD if it is an array.

The resulting array corresponds to **field** with _.true._ elements
of **mask** replaced by values from **vector** in array element order.

### **Examples**
Particular values may be "scattered" to particular positions in an array by using
```text
                       1 0 0
    If M is the array  0 1 0
                       0 0 1

    V is the array [1, 2, 3],
                               . T .
    and Q is the logical mask  T . .
                               . . T
    where "T" represents true and "." represents false, then the result of

    UNPACK (V, MASK = Q, FIELD = M) has the value

      1 2 0
      1 1 0
      0 0 3

    and the result of UNPACK (V, MASK = Q, FIELD = 0) has the value

      0 2 0
      1 0 0
      0 0 3
```

Sample program:

```fortran
program demo_unpack
implicit none
logical,parameter :: T=.true., F=.false.

integer :: vector(2)  = [1,1]

! mask and field must conform
integer,parameter :: r=2, c=2
logical :: mask(r,c)  = reshape([ T,F,F,T ],[2,2])
integer :: field(r,c) = 0, unity(2,2)

   ! basic usage
   unity = unpack( vector, mask, field )
   call print_matrix_int('unity=', unity)

   ! if FIELD is a scalar it is used to fill all the elements
   ! not assigned to by the vector and mask.
   call print_matrix_int('scalar field',         &
   & unpack(                                     &
   & vector=[ 1, 2, 3, 4 ],                      &
   & mask=reshape([ T,F,T,F,F,F,T,F,T ], [3,3]), &
   & field=0) )

contains

   subroutine print_matrix_int(title,arr)
   ! convenience routine:
   ! just prints small integer arrays in row-column format
   implicit none
   character(len=*),intent(in)  :: title
   integer,intent(in)           :: arr(:,:)
   integer                      :: i
   character(len=:),allocatable :: biggest

        write(*,*)trim(title)
        ! make buffer to write integer into
        biggest='           '
        ! find how many characters to use for integers
        write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
        ! use this format to write a row
        biggest='("  [",*(i'//trim(biggest)//':,","))'
        ! print one row of array at a time
        do i=1,size(arr,dim=1)
           write(*,fmt=biggest,advance='no')arr(i,:)
           write(*,'(" ]")')
        enddo
   end subroutine print_matrix_int

end program demo_unpack
```
Results:

```text
   > unity=
   >  [ 1, 0 ]
   >  [ 0, 1 ]
   > scalar field
   >  [  1,  0,  3 ]
   >  [  0,  0,  0 ]
   >  [  2,  0,  4 ]
```

### **Standard**

Fortran 95

### **See Also**

[**merge**(3)](#merge),
[**pack**(3)](#pack),
[**spread**(3)](#spread)

 _fortran-lang intrinsic descriptions_

## verify

### **Name**

**verify**(3) - \[CHARACTER:SEARCH\] Position of a character in a string
of characters that does not appear in a given set of characters.

### **Synopsis**
```fortran
    result = verify(string, set [,back] [,kind] )
```
```fortran
     elemental integer(kind=KIND) function verify(string,set,back,kind)

      character(len=*,kind=**),intent(in) :: string
      character(len=*,kind=**),intent(in) :: set
      logical,intent(in),optional :: back
      integer,intent(in),optional :: KIND
```
### **Characteristics**

 - a kind designated as ** may be any supported kind value for the type.
   However, **string** and **set** must have the same kind type parameter
   for any individual call.
 - the kind of the returned value is the same as **kind** if
   present. Otherwise a default _integer_ kind is returned.
 - **kind** must be a constant _integer_ initialization expression and a
   valid kind for the _integer_ type.

### **Description**

  **verify**(3) verifies that all the characters in **string** belong
  to the set of characters in **set** by identifying the position of
  the first character in the string that is not in the set.

  This makes it easy to verify strings are all uppercase or lowercase,
  follow a basic syntax, only contain printable characters, and many
  of the conditions tested for with the C routines **isalnum**(3c),
  **isalpha**(3c), **isascii**(3c), **isblank**(3c), **iscntrl**(3c),
  **isdigit**(3c), **isgraph**(3c), **islower**(3c), **isprint**(3c),
  **ispunct**(3c), **isspace**(3c), **isupper**(3c), and **isxdigit**(3c);
  but for a string as well an an array of strings.

### **Options**

- **string**
  : The string to search in for an unmatched character.

- **set**
  : The set of characters that must be matched.

- **back**
  : The direction to look for an unmatched character. The left-most
  unmatched character position is returned unless **back** is present
  and _.false._, which causes the position of the right-most unmatched
  character to be returned instead of the left-most unmatched character.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The position of the first or last (if **back** is _.false._) unmatched
character in **string**.

If all characters of **string** are found in **set**, the result is zero.

If **string** is of zero length a zero (0) is always returned.

### **Examples**

#### Sample program I:
```fortran
program demo_verify
implicit none
! some useful character sets
character,parameter :: &
 & int*(*)   = '1234567890', &
 & low*(*)   = 'abcdefghijklmnopqrstuvwxyz', &
 & upp*(*)   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
 & punc*(*)  = "!""#$%&'()*+,-./:;<=>?@[\]^_`{|}~", &
 & blank*(*) = ' ', &
 & tab       = char(11), &
 & prnt*(*) = int//low//upp//blank//punc

character(len=:),allocatable :: string
integer :: i

   ! find first non-uppercase letter
   ! will produce the location of "d", because there is no match in UPP
   write(*,*) 'something unmatched',verify("ABCdEFG", upp)

   ! if everything is matched return zero
   ! will produce 0 as all letters have a match
   write(*,*) 'everything matched',verify("ffoorrttrraann", "nartrof")

   ! easy C-like functionality but does entire strings not just characters
   write(*,*)'isdigit 123?',verify("123", int) == 0
   write(*,*)'islower abc?',verify("abc", low) == 0
   write(*,*)'isalpha aBc?',verify("aBc", low//upp) == 0
   write(*,*)'isblank aBc dEf?',verify("aBc dEf", blank//tab ) /= 0
   ! check if all printable characters
   string="aB;cde,fgHI!Jklmno PQRSTU vwxyz"
   write(*,*)'isprint?',verify(string,prnt) == 0
   ! this now has a nonprintable tab character in it
   string(10:10)=char(11)
   write(*,*)'isprint?',verify(string,prnt) == 0

   ! verify(3f) is often used in a logical expression
   string=" This is NOT all UPPERCASE "
   write(*,*)'all uppercase/spaces?',verify(string, blank//upp) == 0
   string=" This IS all uppercase "
   write(*,*) 'string=['//string//']'
   write(*,*)'all uppercase/spaces?',verify(string, blank//upp) == 0

  ! set and show complex string to be tested
   string='  Check this out. Let me know  '
   ! show the string being examined
   write(*,*) 'string=['//string//']'
   write(*,*) '        '//repeat(int,4) ! number line

   ! the Fortran functions returns a position just not a logical like C
   ! which can be very useful for parsing strings
   write(*,*)'first non-blank character',verify(string, blank)
   write(*,*)'last non-blank character',verify(string, blank,back=.true.)
   write(*,*)'first non-letter non-blank',verify(string,low//upp//blank)

  !VERIFY(3) is elemental so you can check an array of strings in one call
   ! are strings all letters (or blanks)?
   write(*,*) 'array of strings',verify( &
   ! strings must all be same length, so force to length 10
   & [character(len=10) :: "YES","ok","000","good one","Nope!"], &
   & low//upp//blank) == 0

   ! rarer, but the set can be an array, not just the strings to test
   ! you could do ISPRINT() this (harder) way :>
   write(*,*)'isprint?',.not.all(verify("aBc", [(char(i),i=32,126)])==1)
   ! instead of this way
   write(*,*)'isprint?',verify("aBc",prnt) == 0

end program demo_verify
```
Results:
```text
    > something unmatched           4
    > everything matched           0
    > isdigit 123? T
    > islower abc? T
    > isalpha aBc? T
    > isblank aBc dEf? T
    > isprint? T
    > isprint? F
    > true if all uppercase/spaces: F
    > string=[ THIS IS ALL UPPERCASE ]
    > true if all uppercase/spaces: T
    > string=[  Check this out. Let me know  ]
    >        1234567890123456789012345678901234567890
    > first non-blank character            3
    > last non-blank character           29
    > first non-letter non-blank           17
    > array of strings T T F T F
    > isprint? T
    > isprint? T
```
#### Sample program II:

Determine if strings are valid integer representations

```fortran
program fortran_ints
implicit none
integer :: i
character(len=*),parameter :: ints(*)=[character(len=10) :: &
 '+1 ', &
 '3044848 ', &
 '30.40 ', &
 'September ', &
 '1 2 3', &
 '  -3000 ', &
 ' ']
   ! show the strings to test
   write(*,'("|",*(g0,"|"))') ints
   ! show if strings pass or fail the test done by isint(3f)
   write(*,'("|",*(1x,l1,8x,"|"))') isint(ints)

contains

elemental function isint(line) result (lout)
!
! determine if string is a valid integer representation
! ignoring trailing spaces and leading spaces
!
character(len=*),parameter   :: digits='0123456789'
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   lout=.false.
   ! make sure at least two characters long to simplify tests
   name=adjustl(line)//'  '
   ! blank string
   if( name == '' )return
   ! allow one leading sign
   if( verify(name(1:1),'+-') == 0 ) name=name(2:)
   ! was just a sign
   if( name == '' )return
   lout=verify(trim(name), digits)  == 0
end function isint

end program fortran_ints
```
Results:
```text
|+1       |3044848  |30.40    |September|1 2 3    |  -3000  |         |
| T       | T       | F       | F       | F       | T       | F       |
```
#### Sample program III:

Determine if strings represent valid Fortran symbol names

```fortran
program fortran_symbol_name
implicit none
integer :: i
character(len=*),parameter :: symbols(*)=[character(len=10) :: &
 'A_ ', &
 '10 ', &
 'September ', &
 'A B', &
 '_A ', &
 ' ']

   write(*,'("|",*(g0,"|"))') symbols
   write(*,'("|",*(1x,l1,8x,"|"))') fortran_name(symbols)

contains

elemental function fortran_name(line) result (lout)
!
! determine if a string is a valid Fortran name
! ignoring trailing spaces (but not leading spaces)
!
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name).ne.0)then
      ! first character is alphameric
      lout = verify(name(1:1), lower//upper) == 0  &
       ! other characters are allowed in a symbol name
       & .and. verify(name,allowed) == 0           &
       ! allowable length
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name

end program fortran_symbol_name
```
Results:
```text
    |A_        |10        |September |A B       |_A        |          |
    | T        | F        | T        | F        | F        | F        |
```
#### Sample program IV:

check if string is of form NN-HHHHH

```fortran
program checkform
! check if string is of form NN-HHHHH
implicit none
character(len=*),parameter :: int='1234567890'
character(len=*),parameter :: hex='abcdefABCDEF0123456789'
logical                    :: lout
character(len=80)          :: chars

   chars='32-af43d'
   lout=.true.

   ! are the first two characters integer characters?
   lout = lout.and.(verify(chars(1:2), int) == 0)

   ! is the third character a dash?
   lout = lout.and.(verify(chars(3:3), '-') == 0)

   ! is remaining string a valid representation of a hex value?
   lout = lout.and.(verify(chars(4:8), hex) == 0)

   if(lout)then
      write(*,*)trim(chars),' passed'
   else
      write(*,*)trim(chars),' failed'
   endif
end program checkform
```
Results:
```text
    32-af43d passed
```
#### Sample program V:

exploring uses of elemental functionality and dusty corners

```fortran
program more_verify
implicit none
character(len=*),parameter :: &
  & int='0123456789', &
  & low='abcdefghijklmnopqrstuvwxyz', &
  & upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
  & blank=' '
! note character variables in an array have to be of the same length
character(len=6) :: strings(3)=["Go    ","right ","home! "]
character(len=2) :: sets(3)=["do","re","me"]

  ! elemental -- you can use arrays for both strings and for sets

   ! check each string from right to left for non-letter/non-blank
   write(*,*)'last non-letter',verify(strings,upp//low//blank,back=.true.)

   ! even BACK can be an array
   ! find last non-uppercase character in "Howdy "
   ! and first non-lowercase in "there "
   write(*,*) verify(strings(1:2),[upp,low],back=[.true.,.false.])

   ! using a null string for a set is not well defined. Avoid it
   write(*,*) 'null',verify("for tran ", "", .true.) ! 8,length of string?
   ! probably what you expected
   write(*,*) 'blank',verify("for tran ", " ", .true.) ! 7,found 'n'

   ! first character in  "Go    " not in "do",
   ! and first letter in "right " not in "ri"
   ! and first letter in "home! " not in "me"
   write(*,*) verify(strings,sets)

end program more_verify
```
Results:
```text
    > last non-letter 0 0 5
    > 6 6
    > null 9
    > blank 8
    > 1 2 1
```
### **Standard**

Fortran 95 , with **kind** argument - Fortran 2003

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](#adjustl),
  [**adjustr**(3)](#adjustr),
  [**index**(3)](#index),
  [**scan**(3)](#scan),
  [**verify**(3)](#verify)

- **Nonelemental:**
  [**len_trim**(3)](#len_trim),
  [**len**(3)](#len),
  [**repeat**(3)](#repeat),
  [**trim**(3)](#trim)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
