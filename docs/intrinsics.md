
## abs

### **Name**

**abs**(3) - \[NUMERIC\] Absolute value

### **Syntax**

```fortran
  result = abs(a)

   TYPE(kind=KIND) elemental function abs(a)

   TYPE(kind=KIND),intent(in) :: a
```

   where the TYPE and KIND is determined by the type and type attributes
   of **a**, which may be any _real_, _integer_, or _complex_ value.

   If the type of **a** is _complex_ the type returned will be _real_
   with the same kind as the _real_ part of the input value.

   Otherwise the returned type will be the same type as **a**.

### **Description**

   **abs(a)** computes the absolute value of numeric argument **a**.

   In mathematics, the absolute value or modulus of a real number **x**,
   denoted **|x|**, is the magnitude of **x** without regard to its sign.

   The absolute value of a number may be thought of as its distance from
   zero, which is the definition used by **abs**(3) when dealing with
   _complex_ values (_see below_).

### **Arguments**

- **a**
  : the type of the argument shall be an _integer_, _real_, or _complex_
  scalar or array.

### **Returns**

   If **a** is of type _integer_ or _real_, the value of the result is
   **|a|** and of the same type and kind as the input argument.

   (Take particular note) if **a** is _complex_ with value **(x, y)**,
   the result is a _real_ equal to a processor-dependent approximation to
   **sqrt(x\*\*2 + y\*\*2)** computed without undue overflow or underflow.

### **Examples**

Sample program:

```fortran
program demo_abs
implicit none
integer           :: i = -1
real              :: x = -1.0
complex           :: z = (-3.0,-4.0)
doubleprecision   :: rr = -45.78d+00
character(len=*),parameter :: &
 frmt =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
 frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)'
integer,parameter :: dp=kind(0.0d0)

  ! any integer, real, or complex type
    write(*, frmt)  'integer         ',  i, abs(i)
    write(*, frmt)  'real            ',  x, abs(x)
    write(*, frmt)  'doubleprecision ', rr, abs(rr)
    write(*, frmtc) 'complex         ',  z, abs(z)

  ! any value whose positive value is representable
  ! A dusty corner is that abs(-huge(0)-1) of an integer would input
  ! a representable negative value on most machines but result in a
  ! positive value out of range.
    write(*, *) 'abs range test : ', abs(huge(0)), abs(-huge(0))
    write(*, *) 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
    write(*, *) 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

  ! elemental
    write(*, *) 'abs is elemental: ', abs([20,  0,  -1,  -3,  100])

  ! complex input produces real output
    write(*, *)  cmplx(30.0,40.0)

  ! the returned value for complex input can be thought of as the
  ! distance from the origin <0,0>
    write(*, *) 'distance of <XX,YY> from zero is', &
               & distance(30.0_dp,40.0_dp)

    contains

    real(kind=dp) elemental function distance(x,y)
    real(kind=dp),intent(in) :: x,y
       ! dusty corners:
       ! note that KIND=DP is NOT optional
       ! if the desired result is KIND=dp.
       ! See cmplx(3).
       distance=abs( cmplx(x,y,kind=dp) )
    end function distance

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
    (30.00000,40.00000)
    distance of <XX,YY> from zero is   50.0000000000000
```

### **Standard**

   FORTRAN 77 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## achar

### **Name**

**achar**(3) - \[CHARACTER:CONVERSION\] returns a character in a specified position in the ASCII collating sequence

### **Syntax**

```fortran
  result = achar(i,kind=KIND)

    character(len=1) elemental function :: achar(i,kind=KIND)

    integer(kind=KIND),intent(in) :: i
    integer(kind=KIND),intent(in),optional :: kind
```

where KIND may be any supported kind value for _integer_ types.

### **Description**

**achar(i)** returns the character located at position **i** (commonly called the
_ADE_ or ASCII Decimal Equivalent) in the ASCII collating sequence.

The **achar**(3) function is often used for generating in-band escape
sequences to control terminal attributes.

```fortran
   write(*,'(*(a))')achar(27),'[2J'
```

will clear the screen on an ANSI-compatible terminal display, for
example.

### **Arguments**

- **i**
  : the _integer_ value to convert to an ASCII character, in the range
  0 to 127.

- **kind**
  : (optional) an _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is the requested character of type _character_ with a
length of one. If the **kind** argument is present, the return value is of
the specified kind and of the default kind otherwise.

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

elemental pure function upper(str) result (string)
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

FORTRAN 77 and later, with KIND argument Fortran 2003 and later

### **See Also**

[**char**(3)](CHAR),
[**iachar**(3)](IACHAR),
[**ichar**(3)](ICHAR)

### **Resources**

- [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [M_attr module](https://github.com/urbanjost/M_attr) for controlling ANSI-compatible terminals

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## acosh

### **Name**

**acosh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic cosine function

### **Syntax**

```fortran
  result = acosh(x)

   TYPE(kind=KIND),elemental :: acosh

   TYPE(kind=KIND,intent(in) :: x
```

where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

### **Description**

**acosh(x)** computes the inverse hyperbolic cosine of **x** in radians.

### **Arguments**

- **x**
  : the type shall be _real_ or _complex_.

### **Returns**

The return value has the same type and kind as **x**.

If **x** is _complex_, the imaginary part of the result is in radians and
lies between

> **0 \<= aimag(acosh(x)) \<= PI**

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

Fortran 2008 and later

### **See Also**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [**cosh**(3)](COSH)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## acos

### **Name**

**acos**(3) - \[MATHEMATICS:TRIGONOMETRIC\] arccosine (inverse cosine) function

### **Syntax**

```fortran
  result = acos(x)

   TYPE(kind=KIND),elemental :: acos

   TYPE(kind=KIND,intent(in) :: x
```

where **TYPE** may be _real_ or _complex_ and **KIND** may be any **KIND** supported
by the associated type.

### **Description**

**acos(x)** computes the arccosine of **x** (inverse of **cos(x)**).

### **Arguments**

- **x**
  : Must be type _real_ or _complex_. If the type is _real_, the value
  must satisfy |**x**| <= 1.

### **Returns**

The return value is of the same type and kind as **x**. The _real_ part of
the result is in radians and lies in the range **0 \<= acos(x%re) \<= PI** .

### **Examples**

Sample program:

```fortran
program demo_acos
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x = 0.866_real64
real(kind=real64),parameter :: d2r=acos(-1.0_real64)/180.0_real64

    print all,'acos(',x,') is ', acos(x)
    print all,'90 degrees is ', d2r*90.0_real64, ' radians'
    print all,'180 degrees is ', d2r*180.0_real64, ' radians'
    print all,'for reference &
    &PI ~ 3.14159265358979323846264338327950288419716939937510'
    print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])

end program demo_acos
```

Results:

```text
   acos( .8660000000000000 ) is  .5236495809318289
   90 degrees is  1.570796326794897  radians
   180 degrees is  3.141592653589793  radians
   for reference PI ~ 3.14159265358979323846264338327950288419716939937510
   elemental 3.141593 2.094395 1.570796 1.047198 .000000
```

### **Standard**

FORTRAN 77 and later; for a _complex_ argument - Fortran 2008 and later

### **See Also**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [**cos**(3](COS))

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## adjustl

### **Name**

**adjustl**(3) - \[CHARACTER:WHITESPACE\] Left-adjust a string

### **Syntax**

```fortran
    result = adjustl(string)

     character(len=(len(string)) elemental function adjustr(a)

     character(len=*),intent(in) :: string
```

### **Description**

**adjustl(string)** will left-adjust a string by removing leading
spaces. Spaces are inserted at the end of the string as needed.

### **Arguments**

- **string**
  : the type shall be _character_.

### **Returns**

The return value is of type _character_ and of the same kind as **string**
where leading spaces are removed and the same number of spaces are
inserted on the end of **string**.

### **Examples**

Sample program:

```fortran
program demo_adjustl
implicit none
character(len=20) :: str = '   sample string'
character(len=:),allocatable :: astr
    !
    ! basic use
    str = adjustl(str)
    write(*,'("[",a,"]")') str, trim(str)
    !
    ! an allocatable string stays the same length
    ! and is not trimmed.
    astr='    allocatable string   '
    write(*,'("[",a,"]")') adjustl(astr)
    !
end program demo_adjustl
```

Results:

```text
   [sample string       ]
   [sample string]
   [allocatable string       ]
```

### **Standard**

Fortran 95 and later

### **See Also**

[**adjustr**(3)](ADJUSTR)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## adjustr

### **Name**

**adjustr**(3) - \[CHARACTER:WHITESPACE\] Right-adjust a string

### **Syntax**

```fortran
    result = adjustr(string)

     elemental function adjustr(a)
     character(len=(len(string)) :: adjustr
     character(len=*),intent(in) :: string
```

### **Description**

**adjustr(string)** right-adjusts a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed to
retain the original length.

### **Arguments**

- **string**
  : the type shall be _character_.

### **Returns**

The return value is of type _character_ and of the same kind as **string**
where trailing spaces are removed and the same number of spaces are
inserted at the start of **string**.

### **Examples**

Sample program:

```fortran
program demo_adjustr
implicit none
character(len=20) :: str = ' sample string '
   ! print a short number line
   write(*,'(a)')repeat('1234567890',5)

   !
   ! basic usage
   !
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])

   write(*,'(a)')repeat('1234567890',5)
end program demo_adjustr
```

Results:

```text
   12345678901234567890123456789012345678901234567890
          sample string
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```

### **Standard**

Fortran 95 and later

### **See Also**

[**adjustl**(3)](ADJUSTL)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## aimag

### **Name**

**aimag**(3) - \[TYPE:NUMERIC\] Imaginary part of complex number

### **Syntax**

```fortran
    result = aimag(z)

     complex(kind=KIND),elemental :: aimag

     complex(kind=KIND),intent(in) :: z
```
### **Description**

**aimag(z)** yields the imaginary part of complex argument **z**.

This is similar to the modern complex-part-designator **%IM** which also
designates the imaginary part of a value, accept a designator can appear
on the left-hand side of an assignment as well, as in **val%im=10.0**.

### **Arguments**

- **z**
  : The type of the argument shall be _complex_.

### **Returns**

The return value is of type _real_ with the kind type parameter of the
argument.

### **Examples**

Sample program:

```fortran
program demo_aimag
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
complex(kind=real32) z4
complex(kind=real64) z8
    z4 = cmplx(1.e0, 2.e0)
    z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
    print *, aimag(z4), aimag(z8)
    ! an elemental function can be passed an array
    print *
    print *, [z4,z4/2.0,z4+z4,z4**3]
    print *
    print *, aimag([z4,z4/2.0,z4+z4,z4**3])
end program demo_aimag
```
Results:
```text
  2.000000       4.00000000000000

 (1.000000,2.000000) (0.5000000,1.000000) (2.000000,4.000000)
 (-11.00000,-2.000000)

       2.000000       1.000000       4.000000      -2.000000
```
### **See Also**

[**real**(3)](REAL),
[**cmplx**(3)](CMPLX)

### **Standard**

FORTRAN 77 and later

_fortran-lang intrinsic descriptions_

## aint

### **Name**

**aint**(3) - \[NUMERIC\] Truncate to a whole number

### **Syntax**

```fortran
result = aint(x)

   real(kind=kind(x)),elemental  :: aint

   real(kind=kind(x)),intent(in) :: x
```

or

```fortran
result = aint(x, KIND)

   real(kind=KIND),elemental     :: aint

   integer,intent(in),optional   :: KIND
   real(kind=kind(x)),intent(in) :: x
```

### **Description**

**aint(x, kind)** truncates its argument to a whole number.

### **Arguments**

- **x**
  : the type of the argument shall be _real_.

- **kind**
  : (optional) an _integer_ initialization expression indicating the
  kind parameter of the result.

### **Returns**

The return value is of type _real_ with the kind type parameter of
the argument if the optional **kind** is absent; otherwise, the kind
type parameter will be given by **kind**. If the magnitude of **x**
is less than one, **aint(x)** returns zero. If the magnitude is equal
to or greater than one then it returns the largest whole number that
does not exceed its magnitude. The sign is the same as the sign of **x**.

### **Examples**

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : real32, real64
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 4.3210_real32
   x8 = 4.3210_real64
   print *, aint(x4), aint(x8)
   print *
   ! elemental
   print *,aint([ &
    &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0,   &
    &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_aint
```

Results:

```text
     4.00000000       4.0000000000000000

    -2.00000000      -2.00000000      -2.00000000      -2.00000000
    -1.00000000      -1.00000000      -0.00000000       0.00000000
     0.00000000       1.00000000       1.00000000       2.00000000
     2.00000000       2.00000000       2.00000000
```

### **Standard**

FORTRAN 77 and later

### **See Also**

[**anint**(3)](ANINT),
[**int**(3)](INT),
[**nint**(3)](NINT),
[**selected_int_kind**(3)](SELECTED_INT_KIND),
[**ceiling**(3)](CEILING),
[**floor**(3)](FLOOR)

_fortran-lang intrinsic descriptions_

## all

### **Name**

**all**(3) - \[ARRAY REDUCTION\] determines if all the values are true

### **Syntax**

```fortran
result = all(mask, dim)
```

### **Description**

Logical conjunction of elements of **mask** along dimension **dim**.

"**all(mask, dim)**" determines if all the values are true in **mask**
in the array along dimension **dim**.

### **Arguments**

- **mask**
  : shall be a logical array.

- **dim**
  : (optional) **dim** shall be a scalar integer with a value that lies
  between one and the rank of **mask**. The corresponding actual argument
  shall not be an optional dummy argument.

### **Returns**

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
### **See Also**
[**any**(3)](ANY)

### **Standard**
Fortran 95 and later

_fortran-lang intrinsic descriptions_

## allocated

### **Name**

**allocated**(3) - \[ARRAY INQUIRY\] Status of an allocatable entity

### **Syntax**
```fortran
  result = allocated(array)
```
or
```fortran
  result = allocated(scalar)
```
### **Description**

**allocated(array)** and **allocated(scalar)** check the allocation
status of **array** and **scalar**, respectively.

### **Arguments**

- **array**
  : the argument shall be an _allocatable_ array or allocatable scalar.

- **scalar**
  : the argument shall be an _allocatable_ scalar.

### **Returns**

The return value is a scalar _logical_ with the default logical kind type
parameter. If the argument is allocated then the result is _.true._;
otherwise, it returns _.false._.

### **Examples**

Sample program:

```fortran
program demo_allocated
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
integer :: i = 4
real(kind=sp), allocatable :: x(:)
character(len=256) :: message
integer :: istat

   ! if already allocated, deallocate
   if ( allocated(x) ) deallocate(x,STAT=istat, ERRMSG=message )
   if(istat.ne.0)then
      write(*,*)trim(message)
      stop
   endif

   ! only if not allocated, allocate
   if ( .not. allocated(x) ) allocate(x(i))

   write(*,*)allocated(x), size(x)
   if( allocated(x)) then
       write(*,*)'do things if allocated'
   else
       write(*,*)'do things if not allocated'
   endif

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

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

### **See Also**

[**move_alloc**(3)](MOVE_ALLOC)

_fortran-lang intrinsic descriptions_
SYNOPSIS

## anint

### **Name**

**anint**(3) - \[NUMERIC\] Nearest whole number

### **Syntax**

```fortran
result = anint(a, kind)
```

### **Description**

**anint(a \[, kind\])** rounds its argument to the nearest whole number.

### **Arguments**

- **a**
  : the type of the argument shall be _real_.

- **kind**
  : (optional) an _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type real with the kind type parameter of the
argument if the optional **kind** is absent; otherwise, the kind type
parameter will be given by **kind**. If **a** is greater than zero, **anint(a)**
returns **aint(a + 0.5)**. If **a** is less than or equal to zero then it
returns **aint(a - 0.5)**.

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

FORTRAN 77 and later

### **See Also**

[**aint**(3)](AINT),
[**int**(3)](INT),
[**nint**(3)](NINT),
[**selected_int_kind**(3)](SELECTED_INT_KIND),
[**ceiling**(3)](CEILING),
[**floor**(3)](FLOOR)

_fortran-lang intrinsic descriptions_

## any

### **Name**

**any**(3) - \[ARRAY REDUCTION\] determines if any of the values in the logical array are true.

### **Syntax**

```fortran
result = any(mask, dim)
```

### **Description**

**any(mask, dim)** determines if any of the values in the logical
array **mask** along dimension **dim** are **.true.**.

### **Arguments**

- **mask**
  : the type of the argument shall be _logical_ and it shall not be
  scalar.

- **dim**
  : (optional) dim shall be a scalar integer with a value that lies
  between one and the rank of mask.

### **Returns**

**any(mask)** returns a scalar value of type _logical_ where the kind type
parameter is the same as the kind type parameter of **mask**. If **dim** is
present, then **any(mask, dim)** returns an array with the rank of **mask**
minus 1. The shape is determined from the shape of **mask** where the **dim**
dimension is elided.

1.  **any(mask)** is true if any element of **mask** is true; otherwise, it
    is **.false.**. It also is false if **mask** has zero size.

2.  If the rank of **mask** is one, then **any(mask, dim)** is equivalent to
    **any(mask)**. If the rank is greater than one, then **any(mask,
    dim)** is determined by applying **any()** to the array sections.

### **Examples**

Sample program:

```fortran
program demo_any
implicit none
logical l
   l = any([.true., .true., .true.])
   print *, l
   call section
   contains
     subroutine section
     integer a(2,3), b(2,3)
       a = 1
       b = 1
       b(2,2) = 2
       print *, any(a .eq. b, 1)
       print *, any(a .eq. b, 2)
     end subroutine section
end program demo_any
```

Results:

```text
    T
    T T T
    T T
```
### **See Also**
[**any**(3)](ALL)

### **Standard**

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## asinh

### **Name**

**asinh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function

### **Syntax**

```fortran
result = asinh(x)

    elemental TYPE(kind=KIND) function asinh(x)
    TYPE(kind=KIND) :: x
```

Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

### **Description**

**asinh(x)** computes the inverse hyperbolic sine of **x**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

The return value is of the same type and kind as **x**. If **x** is _complex_, the
imaginary part of the result is in radians and lies between
**-PI/2 \<= aimag(asinh(x)) \<= PI/2**.

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

Fortran 2008 and later

### **See Also**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [**sinh**(3)](SINH)

_fortran-lang intrinsic descriptions_

## asin

### **Name**

**asin**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arcsine function

### **Syntax**

```fortran
result = asin(x)

    elemental TYPE(kind=KIND) function asin(x)
    TYPE(kind=KIND) :: x
```

where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

### **Description**

**asin(x)** computes the arcsine of its argument **x**.

The arcsine is the inverse function of the sine function. It is commonly
used in trigonometry when trying to find the angle when the lengths of
the hypotenuse and the opposite side of a right triangle are known.

### **Arguments**

- **x**
  : The type shall be either _real_ and a magnitude that is less than or
  equal to one; or be _complex_.

### **Returns**

- **result**
  : The return value is of the same type and kind as **x**. The real part of
  the result is in radians and lies in the range **-PI/2 \<=
  asin(x) \<= PI/2**.

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

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

### **See Also**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [**sin**(3)](SIN)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## associated

### **Name**

**associated**(3) - \[STATE\] Status of a pointer or pointer/target pair

### **Syntax**

```fortran
result = associated(pointer, target)
```

### **Description**

**associated(pointer \[, target\])** determines the status of the
pointer **pointer** or if **pointer** is associated with the target **target**.

### **Arguments**

- **pointer**
  : **pointer** shall have the _pointer_ attribute and it can be of any type.

- **target**
  : (Optional) **target** shall be a pointer or a target. It must have the
  same type, kind type parameter, and array rank as **pointer**.

The association status of neither **pointer** nor **target** shall be undefined.

### **Returns**

**associated(pointer)** returns a scalar value of type _logical_.
There are several cases:

1.  When the optional **target** is not present then **associated(pointer)**
    is true if **pointer** is associated with a target; otherwise, it
    returns false.

2.  If **target** is present and a scalar target, the result is true if
    **target** is not a zero-sized storage sequence and the target
    associated with **pointer** occupies the same storage units. If **pointer**
    is disassociated, the result is false.

3.  If **target** is present and an array target, the result is true if
    **target** and **pointer** have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    **target** and **pointer** occupy the same storage units in array element
    order.

    As in case 2, the result is false, if **pointer** is disassociated.

4.  If **target** is present and an scalar pointer, the result is true if
    **target** is associated with **pointer**, the target associated with **target**
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is **.false.**, if either **target** or **pointer** is disassociated.

5.  If **target** is present and an array pointer, the result is true if
    target associated with **pointer** and the target associated with **target**
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and **target** and
    **pointer** occupy the same storage units in array element order. The
    result is false, if either **target** or **pointer** is disassociated.

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

Fortran 95 and later

### **See Also**

[**null**(3)](NULL)

_fortran-lang intrinsic descriptions_

## atan2

### **Name**
**atan2**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent (inverse tangent)
function

### **Syntax**
```fortran
  elemental function atan2(y, x)

    type(real,kind=KIND) :: atan2
    type(real,kind=KIND),intent(in) :: y, x
```
The return value has the same type and kind as **y**
and **x**.

### **Description**

**atan2(y, x)** computes in radians a processor-dependent approximation of
the arctangent of the complex number ( **x**, **y** ) or equivalently the
principal value of the arctangent of the value **y/x** (which determines
a unique angle).

### **Arguments**

- **y**
  : The imaginary component of the complex value **(x,y)** or the **y**
  component of the point **\<x,y\>**.

- **x**
  : The real component of the complex value **(x,y)** or the **x**
  component of the point **\<x,y\>**.

  If must be of the same kind as **y**.

### **Returns**

The value returned is by definition the principal value of the complex
number **(x, y)**.

The principal value is simply what we get when we adjust a radian value
to lie between **-PI** and **PI** inclusive,

The type and kind of the result are the same as the elements of **x**.
and **y**.

The classic definition of the arctangent is the angle that is formed
in Cartesian coordinates of the line from the origin point **\<0,0\>**
to the point **\<x,y\>** .

Pictured as such a vector it is easy to see that if **x** and **y**
are both zero the angle is indeterminent because it sits directly over
the origin, so **atan(0.0,0.0)** will produce an error.

Range of returned values by quadrant:
```text
>                   +PI/2
>                     |
>                     |
>        PI/2<Z<PI    |   0>Z<PI/2
>                     |
>   +-PI -------------+---------------- +-0
>                     |
>         PI/2<-Z<PI  |    0<-Z<PI/2
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
real :: x, y, z
complex :: c

 ! basic usage
  ! ATAN2 (1.5574077, 1.0) has the value 1.0 (approximately).
  z=atan2(1.5574077, 1.0)
  write(*,*) 'radians=',z,'degrees=',r2d(z)

 ! elemental arrays
  write(*,*)'elemental',atan2( [10.0, 20.0], [30.0,40.0] )
 ! elemental arrays and scalars
  write(*,*)'elemental',atan2( [10.0, 20.0], 50.0 )

 ! break into real and imaginary components to use with complex values
 ! note TAN2() can take a complex value
  c=(0.0,1.0)
  write(*,*)'complex',c,atan2( x=c%re, y=c%im )
  COMPLEX_VALS: block
  real                :: ang, radius
  complex,allocatable :: vals(:)

  vals=[ &
    ( 0.0, 1.0 ), &
    ( 1.0, 1.0 ), &
    ( 1.0, 0.0 ), &
    ( 0.0,-1.0 ), &
    (-1.0, 1.0 ), &
    (-1.0, 0.0 ), &
    (-1.0,-1.0 )]
  do i=1,size(vals)
     call cartesian_to_polar(vals(i)%re,vals(i)%im,radius,ang)
     write(*,101)vals(i),ang,r2d(ang),radius
  enddo
  101 format('X= ',f5.2,' Y= ',f5.2,' ANGLE= ',g0,T40,'DEGREES= ',g0.4,T57,'DISTANCE=',g0)
 endblock COMPLEX_VALS

contains

elemental real function r2d(radians)
! input radians to convert to degrees
doubleprecision,parameter :: DEGREE=0.017453292519943d0 ! radians
real,intent(in)           :: radians
   r2d=radians / DEGREE ! do the conversion
end function r2d

subroutine cartesian_to_polar(x,y,radius,inclination)
implicit none
real,intent(in)  :: x,y
real,intent(out) :: radius,inclination
   radius=sqrt(x**2+y**2)
   if(radius.eq.0)then
      inclination=0.0
   else
      inclination=atan2(y,x)
   endif
end subroutine cartesian_to_polar

end program demo_atan2
```
  Results:
```text
> radians=   1.00000000     degrees=   57.2957802
> elemental  0.321750551      0.463647604
> elemental  0.197395563      0.380506366
>X=  0.00 Y=  1.00 ANGLE= 1.57079637    DEGREES= 90.00   DISTANCE=1.00000000
>X=  1.00 Y=  1.00 ANGLE= 0.785398185   DEGREES= 45.00   DISTANCE=1.41421354
>X=  1.00 Y=  0.00 ANGLE= 0.00000000    DEGREES= 0.000   DISTANCE=1.00000000
>X=  0.00 Y= -1.00 ANGLE= -1.57079637   DEGREES= -90.00  DISTANCE=1.00000000
>X= -1.00 Y=  1.00 ANGLE= 2.35619450    DEGREES= 135.0   DISTANCE=1.41421354
>X= -1.00 Y=  0.00 ANGLE= 3.14159274    DEGREES= 180.0   DISTANCE=1.00000000
>X= -1.00 Y= -1.00 ANGLE= -2.35619450   DEGREES= -135.0  DISTANCE=1.41421354
```
### **See Also**

- [**atan**(3)](ATAN)
- [arctan:wikipedia](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

### **Standard**

FORTRAN 77 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## atanh

### **Name**

**atanh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function

### **Syntax**

```fortran
result = atanh(x)
```

### **Description**

**atanh(x)** computes the inverse hyperbolic tangent of **x**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

The return value has same type and kind as **x**. If **x** is _complex_, the
imaginary part of the result is in radians and lies between

**-PI/2 \<= aimag(atanh(x)) \<= PI/2**

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

Fortran 2008 and later

### **See Also**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [**tanh**(3)](TANH)

_fortran-lang intrinsic descriptions_

## atan

### **Name**

**atan**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

### **Syntax**

```fortran
   result = atan(y, x)

    TYPE(kind=KIND):: atan
    TYPE(kind=KIND,intent(in) :: x
    TYPE(kind=KIND,intent(in),optional :: y
```

where **TYPE** may be _real_ or _complex_ and **KIND** may be any **KIND** supported
by the associated type. If **y** is present **x** is \_real`.

### **Description**

**atan(x)** computes the arctangent of **x**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_; if **y** is present, **x**
  shall be _real_.

- **y**
  : Shall be of the same type and kind as **x**. If **x** is zero, **y**
  must not be zero.

### **Returns**

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

FORTRAN 77 and later for a complex argument; and for two
arguments Fortran 2008 or later

### **See Also**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

[**atan2**(3)](ATAN2), [**tan**(3)](TAN)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## atomic_add

### **Name**

**atomic_add**(3) - \[ATOMIC\] Atomic ADD operation

### **Syntax**

```fortran
call atomic_add (atom, value, stat)
```

### **Description**

**atomic_ad(atom, value)** atomically adds the value of VAR to the
variable **atom**. When **stat** is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_fetch_add**(3)](ATOMIC_FETCH),
[**atomic_and**(3)](ATOMIC_AND),
[**atomic_or**(3)](ATOMIC_OR),
[**atomic_xor**(3)](ATOMIC_XOR)
**iso_fortran_env**(3),

_fortran-lang intrinsic descriptions_

## atomic_and

### **Name**

**atomic_and**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation

### **Syntax**

```fortran
call atomic_and(atom, value, stat)
```

### **Description**

**atomic_and(atom, value)** atomically defines **atom** with the bitwise
**and** between the values of **atom** and **value**. When **stat** is present and the
invocation was successful, it is assigned the value 0. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed **atom**, if the remote image has stopped, it is
assigned the value of iso_fortran_env's stat_stopped_image and if
the remote image has failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_fetch_and**(3)](ATOMIC_FETCH_AND),
[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_ref**(3)](ATOMIC_REF),
[**atomic_cas**(3)](ATOMIC_CAS),
**iso_fortran_env**(3),
[**atomic_add**(3)](ATOMIC_ADD),
[**atomic_or**(3)](ATOMIC_OR),
[**atomic_xor**(3)](ATOMIC_XOR)

_fortran-lang intrinsic descriptions_

## atomic_cas

### **Name**

**atomic_cas**(3) - \[ATOMIC\] Atomic compare and swap

### **Syntax**

```fortran
call atomic_cas (atom, old, compare, new, stat)
```

### **Description**

atomic_cas compares the variable **atom** with the value of **compare**; if the
value is the same, **atom** is set to the value of **new**. Additionally, **old** is
set to the value of **atom** that was used for the comparison. When **stat** is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed **atom**, if the remote image
has stopped, it is assigned the value of iso_fortran_env's
stat_stopped_image and if the remote image has failed, the value
stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_ref**(3)](ATOMIC_REF),
[**iso_fortran_env**(3)]()

_fortran-lang intrinsic descriptions_

## atomic_define

### **Name**

**atomic_define**(3) - \[ATOMIC\] Setting a variable atomically

### **Syntax**

```fortran
call atomic_define (atom, value, stat)

   subroutine atomic_define(atom, value, stat)
   TYPE(kind=KIND) :: atom
   TYPE(kind=KIND) :: value
   integer,intent(out),optional :: stat
```

### **Description**

**atomic_define(atom, value)** defines the variable **atom** with the value
**value** atomically. When **stat** is present and the invocation was
successful, it is assigned the value **0**. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed **atom**, if the remote image has stopped, it is assigned
the value of iso_fortran_env's stat_stopped_image and if the remote
image has failed, the value stat_failed_image.

### **Arguments**

- **atom**
  : Scalar coarray or coindexed variable of either integer type with
  atomic_int_kind kind or logical type with atomic_logical_kind
  kind.

- **value**
  : Scalar of the same type as **atom**. If the kind is different, the value
  is converted to the kind of **atom**.

- **stat**
  : (optional) Scalar default-kind integer variable.

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

Fortran 2008 and later; with **stat**, TS 18508 or later

### **See Also**

[**atomic_ref**(3)](ATOMIC_REF),
[**atomic_cas**(3)](ATOMIC_CAS),
**iso_fortran_env**(3),
[**atomic_add**(3)](ATOMIC_ADD),
[**atomic_and**(3)](ATOMIC_AND),
[**atomic_or**(3)](ATOMIC_OR),
[**atomic_xor**(3)](ATOMIC_XOR)

_fortran-lang intrinsic descriptions_

## atomic_fetch_add

### **Name**

**atomic_fetch_add**(3) - \[ATOMIC\] Atomic ADD operation with prior fetch

### **Syntax**

```fortran
call atomic_fetch_add(atom, value, old, stat)
```

### **Description**

**atomic_fetch_add(atom, value, old)** atomically stores the value of
**atom** in **old** and adds the value of **var** to the variable **atom**. When **stat** is
present and the invocation was successful, it is assigned the value **0**.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed **atom**, if the remote image
has stopped, it is assigned the value of iso_fortran_env's
stat_stopped_image and if the remote image has failed, the value
stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_add**(3)](ATOMIC_ADD),
**iso_fortran_env**(3),

[**atomic_fetch_and**(3)](ATOMIC_FETCH_AND),
[**atomic_fetch_or**(3)](ATOMIC_FETCH_OR),

[**atomic_fetch_xor**(3)](ATOMIC_FETCH_XOR)

_fortran-lang intrinsic descriptions_

## atomic_fetch_and

### **Name**

**atomic_fetch_and**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation with prior fetch

### **Syntax**

```fortran
call atomic_fetch_and(atom, value, old, stat)
```

### **Description**

**atomic_fetch_and(atom, value, old)** atomically stores the value of
**atom** in **old** and defines **atom** with the bitwise AND between the values of
**atom** and **value**. When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_and**(3)](ATOMIC_AND),
[**iso_fortran_env**(3)](),

[**atomic_fetch_add**(3)](ATOMIC_FETCH_ADD),
[**atomic_fetch_or**(3)](ATOMIC_FETCH_OR),

[**atomic_fetch_xor**(3)](ATOMIC_FETCH_XOR)

_fortran-lang intrinsic descriptions_

## atomic_fetch_or

### **Name**

**atomic_fetch_or**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation with prior fetch

### **Syntax**

```fortran
call atomic_fetch_or(atom, value, old, stat)
```

### **Description**

**atomic_fetch_or(atom, value, old)** atomically stores the value of
**atom** in **old** and defines **atom** with the bitwise OR between the values of
**atom** and **value**. When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_or**(3)](ATOMIC_OR),
[**iso_fortran_env**(3)](),

[**atomic_fetch_add**(3)](ATOMIC_FETCH_ADD),
[**atomic_fetch_and**(3)](ATOMIC_FETCH_AND),

[**atomic_fetch_xor**(3)](ATOMIC_FETCH_XOR)

_fortran-lang intrinsic descriptions_

## atomic_fetch_xor

### **Name**

**atomic_fetch_xor**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise XOR operation with prior fetch

### **Syntax**

```fortran
call atomic_fetch_xor (atom, value, old, stat)
```

### **Description**

**atomic_fetch_xor(atom, value, old)** atomically stores the value of
**atom** in **old** and defines **atom** with the bitwise **xor** between the values of
**atom** and **value**. When **stat** is present and the invocation was successful,
it is assigned the value **0**. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
**atom**, if the remote image has stopped, it is assigned the value of
iso_fortran_env's stat_stopped_image and if the remote image has
failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_xor**(3)](ATOMIC_XOR),
[**iso_fortran_env**(3)](),

[**atomic_fetch_add**(3)](ATOMIC_FETCH_ADD),
[**atomic_fetch_and**(3)](ATOMIC_FETCH_AND),

[**atomic_fetch_or**(3)](ATOMIC_FETCH_OR)

_fortran-lang intrinsic descriptions_

## atomic_or

### **Name**

**atomic_or**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation

### **Syntax**

```fortran
call atomic_or(atom, value, stat)
```

### **Description**

**atomic_or(atom, value)** atomically defines **atom** with the bitwise **or**
between the values of **atom** and **value**. When **stat** is present and the
invocation was successful, it is assigned the value **0**. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed **atom**, if the remote image has stopped, it is
assigned the value of iso_fortran_env's stat_stopped_image and if
the remote image has failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_fetch_or**(3)](ATOMIC_FETCH),

[**iso_fortran_env**(3)](),
[**atomic_add**(3)](ATOMIC_ADD),
[**atomic_or**(3)](ATOMIC_OR),

[**atomic_xor**(3)](ATOMIC_XOR)

_fortran-lang intrinsic descriptions_

## atomic_ref

### **Name**

**atomic_ref**(3) - \[ATOMIC\] Obtaining the value of a variable atomically

### **Syntax**

```fortran
call atomic_ref(value, atom, stat)
```

### **Description**

**atomic_ref(value, atom)** atomically assigns the value of the
variable **atom** to **value**. When **stat** is present and the invocation was
successful, it is assigned the value **0**. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed **atom**, if the remote image has stopped, it is assigned
the value of iso_fortran_env's **stat_stopped_image** and if the remote
image has failed, the value **stat_failed_image**.

### **Arguments**

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

Fortran 2008 and later; with STAT, TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_cas**(3)](ATOMIC_CAS),
[**iso_fortran_env**(3)](),

[**atomic_fetch_add**(3)](ATOMIC_ADD),
[**atomic_fetch_and**(3)](ATOMIC_AND),

[**atomic_fetch_or**(3)](ATOMIC_OR),
[**atomic_fetch_xor**(3)](ATOMIC_XOR)

_fortran-lang intrinsic descriptions_

## atomic_xor

### **Name**

**atomic_xor**(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise OR operation

### **Syntax**

```fortran
call atomic_xor(atom, value, stat)
```

### **Description**

**atomic_xor(atom, value)** atomically defines **atom** with the bitwise
**xor** between the values of **atom** and **value**. When **stat** is present and the
invocation was successful, it is assigned the value **0**. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed **atom**, if the remote image has stopped, it is
assigned the value of iso_fortran_env's stat_stopped_image and if
the remote image has failed, the value stat_failed_image.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**atomic_define**(3)](ATOMIC_DEFINE),
[**atomic_fetch_xor**(3)](ATOMIC_FETCH),
[**iso_fortran_env**(3)](),
[**atomic_add**(3)](ATOMIC_ADD),
[**atomic_or**(3)](ATOMIC_OR),
[**atomic_xor**(3)](ATOMIC_XOR)

_fortran-lang intrinsic descriptions_

## bessel_j0

### **Name**

**bessel_j0**(3) - \[MATHEMATICS\] Bessel function of the first kind of order 0

### **Syntax**

```fortran
    result = bessel_j0(x)
```

### **Description**

**bessel_j0(x)** computes the Bessel function of the first kind
of order **0** of **x**.

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

The return value is of type _real_ and lies in the range
**-0.4027 \<= bessel(0,x) \<= 1**. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_besj0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.0_real64
   x = bessel_j0(x)
   write(*,*)x
end program demo_besj0
```

Results:

```text
      1.0000000000000000
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bessel_j1**(3)](BESSEL_J1),
[**bessel_jn**(3)](BESSEL_JN),
[**bessel_y0**(3)](BESSEL_Y0),
[**bessel_y1**(3)](BESSEL_Y1),
[**bessel_yn**(3)](BESSEL_YN)

_fortran-lang intrinsic descriptions_

## bessel_j1

### **Name**

**bessel_j1**(3) - \[MATHEMATICS\] Bessel function of the first kind of order 1

### **Syntax**

```fortran
    result = bessel_j1(x)
```

### **Description**

**bessel_j1(x)** computes the Bessel function of the first kind
of order **1** of **x**.

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

The return value is of type _real_ and lies in the range
**-0.5818 \<= bessel(0,x) \<= 0.5818** . It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_besj1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
   x = bessel_j1(x)
   write(*,*)x
end program demo_besj1
```

Results:

```text
     0.44005058574493350
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bessel_j0**(3)](BESSEL_J0),
[**bessel_jn**(3)](BESSEL_JN),
[**bessel_y0**(3)](BESSEL_Y0),
[**bessel_y1**(3)](BESSEL_Y1),
[**bessel_yn**(3)](BESSEL_YN)

_fortran-lang intrinsic descriptions_

## bessel_jn

### **Name**

**bessel_jn**(3) - \[MATHEMATICS\] Bessel function of the first kind

### **Syntax**

```fortran
  result = bessel_jn(n, x)

  result = bessel_jn(n1, n2, x)
```

### **Description**

**bessel_jn(n, x)** computes the Bessel function of the first
kind of order **n** of **x**. If **n** and **x** are arrays, their ranks and shapes
shall conform.

**bessel_jn(n1, n2, x)** returns an array with the Bessel function\|Bessel functions
of the first kind of the orders **n1** to **n2**.

### **Arguments**

- **n**
  : Shall be a scalar or an array of type _integer_.

- **n1**
  : Shall be a non-negative scalar of type _integer_.

- **n2**
  : Shall be a non-negative scalar of type _integer_.

- **x**
  : Shall be a scalar or an array of type _real_. For
  **bessel_jn(n1, n2, x)** it shall be scalar.

### **Returns**

The return value is a scalar of type _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_besjn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = bessel_jn(5,x)
    write(*,*)x
end program demo_besjn
```

Results:

```text
      2.4975773021123450E-004
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bessel_j0**(3)](BESSEL_J0),
[**bessel_j1**(3)](BESSEL_J1),
[**bessel_y0**(3)](BESSEL_Y0),
[**bessel_y1**(3)](BESSEL_Y1),
[**bessel_yn**(3)](BESSEL_YN)

_fortran-lang intrinsic descriptions_

## bessel_y0

### **Name**

**bessel_y0**(3) - \[MATHEMATICS\] Bessel function of the second kind of order 0

### **Syntax**

```fortran
    result = bessel_y0(x)
```

### **Description**

**bessel_y0(x)** computes the Bessel function of the second
kind of order 0 of **x**.

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

The return value is of type _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_besy0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 0.0_real64
  x = bessel_y0(x)
  write(*,*)x
end program demo_besy0
```

Results:

```text
                    -Infinity
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bessel_j0**(3)](BESSEL_J0),
[**bessel_j1**(3)](BESSEL_J1),
[**bessel_jn**(3)](BESSEL_JN),
[**bessel_y1**(3)](BESSEL_Y1),
[**bessel_yn**(3)](BESSEL_YN)

_fortran-lang intrinsic descriptions_

## bessel_y1

### **Name**

**bessel_y1**(3) - \[MATHEMATICS\] Bessel function of the second kind of order 1

### **Syntax**

```fortran
    result = bessel_y1(x)
```

### **Description**

**bessel_y1(x)** computes the Bessel function of the second
kind of order 1 of **x**.

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

The return value is _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_besy1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 1.0_real64
  write(*,*)x, bessel_y1(x)
end program demo_besy1
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bessel_j0**(3)](BESSEL_J0),
[**bessel_j1**(3)](BESSEL_J1),
[**bessel_jn**(3)](BESSEL_JN),
[**bessel_y0**(3)](BESSEL_Y0),
[**bessel_yn**(3)](BESSEL_YN)

_fortran-lang intrinsic descriptions_

## bessel_yn

### **Name**

**bessel_yn**(3) - \[MATHEMATICS\] Bessel function of the second kind

### **Syntax**

```fortran
  result = bessel_yn(n, x)

  result = bessel_yn(n1, n2, x)
```

### **Description**

**bessel_yn(n, x)** computes the Bessel function of the second
kind of order **n** of **x**. If **n** and **x** are arrays, their ranks and shapes
shall conform.

**bessel_yn(n1, n2, x)** returns an array with the Bessel
function\|Bessel functions of the first kind of the orders **n1** to **n2**.

### **Arguments**

- **n**
  : Shall be a scalar or an array of type _integer_.

- **n1**
  : Shall be a non-negative scalar of type _integer_.

- **n2**
  : Shall be a non-negative scalar of type _integer_.

- **x**
  : Shall be a scalar or an array of type _real_; for
  **bessel_yn(n1, n2, x)** it shall be scalar.

### **Returns**

The return value is _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_besyn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
  write(*,*) x,bessel_yn(5,x)
end program demo_besyn
```

Results:

```text
      1.0000000000000000       -260.40586662581222
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bessel_j0**(3)](BESSEL_J0),
[**bessel_j1**(3)](BESSEL_J1),
[**bessel_jn**(3)](BESSEL_JN),
[**bessel_y0**(3)](BESSEL_Y0),
[**bessel_y1**(3)](BESSEL_Y1)

_fortran-lang intrinsic descriptions_

## bge

### **Name**

**bge**(3) - \[BIT:COMPARE\] Bitwise greater than or equal to

### **Syntax**

```fortran
    result = bge(i, j)
```

### **Description**

Determines whether an integer is bitwise greater than or equal to
another.

### **Arguments**

- **i**
  : Shall be of _integer_ type.

- **j**
  : Shall be of _integer_ type, and of the same kind as **i**.

### **Returns**

The return value is of type _logical_ and of the default kind.

### **Standard**

Fortran 2008 and later

### **See Also**

[**bgt**(3)](BGT),
[**ble**(3)](BLE),
[**blt**(3)](BIT)

_fortran-lang intrinsic descriptions_

## bgt

### **Name**

**bgt**(3) - \[BIT:COMPARE\] Bitwise greater than

### **Syntax**

```fortran
    result = bgt(i, j)
```

### **Description**

Determines whether an integer is bitwise greater than another.

### **Arguments**

- **i**
  : Shall be of _integer_ type or a BOZ literal constant.

- **j**
  : Shall be of _integer_ type, and of the same kind as **i**; or a BOZ
  literal constant.

### **Returns**

The return value is of type _logical_ and of the default kind. The result
is true if the sequence of bits represented by _i_ is greater than the
sequence of bits represented by _j_, otherwise the result is false.

### **Standard**

Fortran 2008 and later

### **See Also**

[**bge**(3)](BGE),
[**ble**(3)](BLE),
[**blt**(3)](BLT)

_fortran-lang intrinsic descriptions_

## bit_size

### **Name**

**bit_size**(3) - \[BIT:INQUIRY\] Bit size inquiry function

### **Syntax**

```fortran
    result = bit_size(i)

     function(kind=KIND) :: bit_size
     integer(kind=KIND),intent(in) :: ii
```

### **Description**

**bit_size(i)** returns the number of bits (integer precision plus sign
bit) represented by the type of the _integer_ **i**. **i** can be a
scalar or an array.

### **Arguments**

- **i**
  : An _integer_ value of any kind to determine the size of in bits.
  Because only the type of the argument is examined, the argument need
  not be defined.

### **Returns**

Returns the number of bits used to represent a value of the type
of __i__.  The result is a _integer_ scalar of the same kind as __i__.

### **Examples**

Sample program:

```fortran
program demo_bit_size
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer(kind=int64)          :: answer
character(len=*),parameter   :: fmt='(*(g0,1x))'
    write(*,fmt)'default integer size is',bit_size(0),'bits'
    write(*,fmt)bit_size(bit_size(0_int8)), 'which is kind=',kind(0_int8)
    write(*,fmt)bit_size(bit_size(0_int16)),'which is kind=',kind(0_int16)
    write(*,fmt)bit_size(bit_size(0_int32)),'which is kind=',kind(0_int32)
    write(*,fmt)bit_size(bit_size(0_int64)),'which is kind=',kind(0_int64)

    ! Check size of value not explicitly defined.
    write(*,fmt) int(bit_size(answer))
end program demo_bit_size
```

Typical Results:

```text
   default integer size is 32 bits
   8 which is kind= 1
   16 which is kind= 2
   32 which is kind= 4
   64 which is kind= 8
   64
```

### **Standard**

Fortran 95 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## ble

### **Name**

**ble**(3) - \[BIT:COMPARE\] Bitwise less than or equal to

### **Syntax**

```fortran
    result = ble(i, j)
```

### **Description**

Determines whether an integer is bitwise less than or equal to another.

### **Arguments**

- **i**
  : Shall be of _integer_ type.

- **j**
  : Shall be of _integer_ type, and of the same kind as **i**.

### **Returns**

The return value is of type _logical_ and of the default kind.

### **Standard**

Fortran 2008 and later

### **See Also**

[**bge**(3)](BGE),
[**bgt**(3)](BGT),
[**blt**(3)](BLT)

_fortran-lang intrinsic descriptions_

## blt

### **Name**

**blt**(3) - \[BIT:COMPARE\] Bitwise less than

### **Syntax**

```fortran
    result = blt(i, j)
```

### **Description**

Determines whether an integer is bitwise less than another.

### **Arguments**

- **i**
  : Shall be of _integer_ type.

- **j**
  : Shall be of _integer_ type, and of the same kind as **i**.

### **Returns**

The return value is of type _logical_ and of the default kind.

### **Standard**

Fortran 2008 and later

### **See Also**

[**bge**(3)](BGE),
[**bgt**(3)](BGT),
[**ble**(3)](BLE)

_fortran-lang intrinsic descriptions_

## btest

### **Name**

**btest**(3) - \[BIT:INQUIRY\] Tests a bit of an _integer_ value.

### **Syntax**

```fortran
   result = btest(i, pos)

    integer(kind=KIND) elemental function btest(i,pos)
    integer,intent(in)  :: i
    logical,intent(out) :: pos
```

where **KIND** is any _integer_ kind supported by the programming environment.

### **Description**

**btest(i,pos)** returns logical **.true.** if the bit at **pos** in **i** is set.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **pos**
  : The bit position to query. it must be a valid position for the
  value **i**; ie. **0 <= pos <= bit_size(i)** .

  A value of zero refers to the least significant bit.

### **Returns**

The result is a _logical_ that has the value **.true.** if bit
position **pos** of **i** has the value **1** and the value
**.false.** if bit **pos** of **i** has the value **0**.

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

Fortran 95 and later

### **See Also**

[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**not**(3)](NOT),
[**mvbits**(3)](MVBITS)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## c_associated

### **Name**

**c_associated**(3) - \[ISO_C_BINDING\] Status of a C pointer

### **Syntax**

```fortran
result = c_associated(c_prt_1, c_ptr_2)
```

### **Description**

**c_associated(c_prt_1\[, c_ptr_2\])** determines the status of the
C pointer c_ptr_1 or if c_ptr_1 is associated with the target
c_ptr_2.

### **Arguments**

- **c_ptr_1**
  : Scalar of the type c_ptr or c_funptr.

- **c_ptr_2**
  : (Optional) Scalar of the same type as c_ptr_1.

### **Returns**

The return value is of type _logical_; it is .false. if either c_ptr_1
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

Fortran 2003 and later

### **See Also**

[**c_loc**(3)](C_LOC),
[**c_funloc**(3)](C_FUNLOC),
**iso_c_binding**(3)

_fortran-lang intrinsic descriptions_

## ceiling

### **Name**

**ceiling**(3) - \[NUMERIC\] Integer ceiling function

### **Syntax**

```fortran
result = ceiling(a, kind)

   integer(kind=KIND) elemental function ceiling(a,kind)
   real(kind=ANY),intent(in)   :: a
   integer,intent(in),optional :: kind
```

### **Description**

**ceiling(a)** returns the least integer greater than or equal to **a**.

### **Arguments**

- **a**
  : The type shall be _real_.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type **integer**(kind) if **kind** is present and a
default-kind _integer_ otherwise.

The result is undefined if it cannot be represented in the specified
integer type.

### **Examples**

Sample program:

```fortran
program demo_ceiling
implicit none
real :: x = 63.29
real :: y = -63.59
   print *, ceiling(x)
   print *, ceiling(y)
   ! elemental
   print *,ceiling([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])
end program demo_ceiling
```

Results:

```text
   64
  -63
   -2      -2      -2      -2      -1      -1
    0       0       1       1       2       2
    3       3       3
```

### **Standard**

Fortran 95 and later

### **See Also**

[**floor**(3)](FLOOR),
[**nint**(3)](NINT)

[**aint**(3)](AINT),
[**anint**(3)](ANINT),
[**int**(3)](INT),
[**selected_int_kind**(3)](SELECTED_INT_KIND)

_fortran-lang intrinsic descriptions_

## c_f_pointer

### **Name**

**c_f_pointer**(3) - \[ISO_C_BINDING\] Convert C into Fortran pointer

### **Syntax**

```fortran
call c_f_pointer(cptr, fptr, shape)
```

### **Description**

**c_f_pointer(cptr, fptr\[, shape\])** Assign the target, the C
pointer, **cptr** to the Fortran pointer **fptr** and specify its shape.

### **Arguments**

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

Fortran 2003 and later

### **See Also**

[**c_loc**(3)](C_LOC),
[**c_f_procpointer**(3)](C_F_PROCPOINTER),
**iso_c_binding**(3)

_fortran-lang intrinsic descriptions_

## c_f_procpointer

### **Name**

**c_f_procpointer**(3) - \[ISO_C_BINDING\] Convert C into Fortran procedure pointer

### **Syntax**

```fortran
call c_f_procpointer(cptr, fptr)
```

### **Description**

**c_f_procpointer(cptr, fptr)** assigns the target of the C function
pointer **cptr** to the Fortran procedure pointer **fptr**.

### **Arguments**

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

Fortran 2003 and later

### **See Also**

[**c_loc**(3)](C_LOC),
[**c_f_pointer**(3)](C_F_POINTER),
**iso_c_binding**(3)

_fortran-lang intrinsic descriptions_

## c_funloc

### **Name**

**c_funloc**(3) - \[ISO_C_BINDING\] Obtain the C address of a procedure

### **Syntax**

```fortran
result = c_funloc(x)
```

### **Description**

**c_funloc(x)** determines the C address of the argument.

### **Arguments**

- **x**
  : Interoperable function or pointer to such function.

### **Returns**

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

Fortran 2003 and later

### **See Also**

[**c_associated**(3)](C_ASSOCIATED),
[**c_loc**(3)](C_LOC),
[**c_f_pointer**(3)](C_F_POINTER),

[**c_f_procpointer**(3)](C_F_PROCPOINTER),
**iso_c_binding**(3)

_fortran-lang intrinsic descriptions_

## char

### **Name**

**char**(3) - \[CHARACTER\] Character conversion function

### **Syntax**

```fortran
result = char(i, kind)
   elemental integer function char(i,kind)

    integer(kind=KIND),intent(in) :: c
    integer,intent(in),optional :: KIND
```

### **Description**

**char(i, kind)** returns the character represented by the integer **i**.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

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

See [**ichar**(3)](CHAR) for a discussion of converting between numerical
values and formatted string representations.

### **Standard**

FORTRAN 77 and later

### **See Also**

[**achar**(3)](ACHAR),
[**iachar**(3)](IACHAR),
[**ichar**(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL), [**adjustr**(3)](ADJUSTR), [**index**(3)](INDEX),
  [**scan**(3)](SCAN), [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT), [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## c_loc

### **Name**

**c_loc**(3) - \[ISO_C_BINDING\] Obtain the C address of an object

### **Syntax**

```fortran
result = c_loc(x)
```

### **Description**

**c_loc(x)** determines the C address of the argument.

### **Arguments**

- **x**
  : Shall have either the _pointer_ or _target_ attribute. It shall not be a
  coindexed object. It shall either be a variable with interoperable
  type and kind type parameters, or be a scalar, nonpolymorphic
  variable with no length type parameters.

### **Returns**

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

Fortran 2003 and later

### **See Also**

[**c_associated**(3)](C_ASSOCIATED),
[**c_funloc**(3)](C_FUNLOC),
[**c_f_pointer**(3)](C_F_POINTER),

[**c_f_procpointer**(3)](C_F_PROCPOINTER),
**iso_c_binding**(3)

_fortran-lang intrinsic descriptions_

## cmplx

### **Name**

**cmplx**(3) - \[TYPE:NUMERIC\] Complex conversion function

### **Syntax**

```fortran
result = cmplx(x, y, kind)

   complex elemental function :: cmplx
   TYPE(kind=KIND),intent(in), x
   TYPE(kind=KIND),intent(in),optional, y
   integer,intent(in),optional :: kind
```

### **Description**

To convert numeric variables to complex, use the **cmplx**(3) function.
Constants can be used to define a complex variable using the syntax

```
      z8 = (1.2345678901234567d0, 1.2345678901234567d0)
```

but this will not work for variables. You must use the **cmplx**(3) function.

**cmplx(x \[, y \[, kind\]\])** returns a complex number where **x** is
converted to the _real_ component. If **x** is _complex_ then **y** must not be
present. If **y** is present it is converted to the imaginary component. If
**y** is not present then the imaginary component is set to **0.0**.

### **cmplx(3) and double precision**

The Fortran 90 language defines **cmplx**(3) as always returning a result
that is type **complex(kind=KIND(0.0))**.

This means **cmplx(d1,d2)**, where **d1** and **d2** are
_doubleprecision_, is treated as:
```fortran
      cmplx(sngl(d1), sngl(d2))
```
_doubleprecision complex_ numbers require specifying a precision.

It was necessary for Fortran 90 to specify this behavior for
_doubleprecision_ arguments, since that is the behavior mandated by
FORTRAN 77.

So Fortran 90 extends the **cmplx**(3) intrinsic by adding an extra
argument used to specify the desired kind of complex result.

```fortran
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
      !
      ! NO: result is just the precision of default _real_ values
      !     because KIND parameter is not specified
      !
      ! note this was stored with default real precision
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note components are just _real_
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      !
      ! YES
      !
      ! kind= makes it work
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
      print *, 'YES, Z8=',z8,real(z8),aimag(z8)
```

F2018 COMPONENT SYNTAX The real and imaginary parts of a complex entity
can be accessed independently with a component-like syntax in f2018:

A complex-part-designator is

```fortran
designator % RE
or
designator % IM.

````

Where the designator is of complex type.

So designator%RE designates the real part of a complex value,
designator%IM designates the imaginary part of complex value. The type
of a complex-part-designator is _real_, and its kind and shape are those
of the designator.

The following are examples of complex part designators:

```fortran
       impedance%re           !-- Same value as _real_(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of x to zero
````

### **Arguments**

- **x**
  The type may be _integer_, _real_, or _complex_.

- **y**
  (Optional; only allowed if **x** is not _complex_.). May be _integer_ or
  _real_.

- **kind**
  (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of _complex_ type, with a kind equal to **kind** if it is
specified. If **kind** is not specified, the result is of the default
_complex_ kind, regardless of the kinds of **x** and **y**.

### **Examples**

Sample program:

```fortran
program demo_aimag
implicit none
integer,parameter :: dp=kind(0.0d0)
complex          :: z4
complex(kind=dp) :: z8
   z4 = cmplx(1.23456789, 1.23456789)
   print *, 'Z4=',z4
   ! using kind=dp makes it keep DOUBLEPRECISION precision
   z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
   print *, 'Z8=',z8
   ! NOTE:
   ! The following is intuitive and works without calling cmplx(3)
   ! but does not work for variables just constants
   z8 = (1.2345678901234567d0, 1.2345678901234567d0 )
   print *, 'Z8 defined with constants=',z8
end program demo_aimag
```

Typical Results:

```
    Z4= (1.23456788,1.23456788)
    Z8= (1.2345678901234567,1.2345678901234567)
    Z8 defined with constants= (1.2345678901234567,1.2345678901234567)
```

### **See Also**

- [**aimag**(3)](AIMAG) - Imaginary part of complex number
- [**conjg**(3)](CONJG) - Complex conjugate function
- [**real**(3)](REAL) - Convert to real type
- [**dble**(3)](DBLE) - Convert to doubleprecision
- [**int**(3)](INT)   - Convert to an integer

### **Standard**

FORTRAN 77 and later

_fortran-lang intrinsic descriptions_

## co_broadcast

### **Name**

**co_broadcast**(3) - \[COLLECTIVE\] Copy a value to all images the current set of images

### **Syntax**

```fortran
call co_broadcast(a, source_image, stat, errmsg)
```

### **Description**

**co_broadcast(3)** copies the value of argument **a** on the image with image
index source_image to all images in the current team. **a** becomes defined
as if by intrinsic assignment. If the execution was successful and **stat**
is present, it is assigned the value zero. If the execution failed, **stat**
gets assigned a nonzero value and, if present, **errmsg** gets assigned a
value describing the occurred error.

### **Arguments**

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

### **See Also**

[**co_max**(3)](CO_MAX),
[**co_min**(3)](CO_MIN),
[**co_sum**(3)](CO_SUM),
[**co_reduce**(3)](CO_REDUCE)

_fortran-lang intrinsic descriptions_

## co_lbound

### **Name**

**co_lbound**(3) - \[COLLECTIVE\] Lower codimension bounds of an array

### **Syntax**

```fortran
result = co_lbound(coarray, dim, kind)
```

### **Description**

Returns the lower bounds of a coarray, or a single lower cobound along
the **dim** codimension.

### **Arguments**

- **array**
  : Shall be an coarray, of any type.

- **dim**
  : (Optional) Shall be a scalar _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is absent, the
result is an array of the lower cobounds of **coarray**. If **dim** is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

### **Standard**

Fortran 2008 and later

### **See Also**

[**co_ubound**(3)](CO_UBOUND),
[**lbound**(3)](LBOUND)

_fortran-lang intrinsic descriptions_

## co_max

### **Name**

**co_max**(3) - \[COLLECTIVE\] Maximal value on the current set of images

### **Syntax**

```fortran
call co_max(a, result_image, stat, errmsg)
```

### **Description**

co_max determines element-wise the maximal value of **a** on all images of
the current team. If result_image is present, the maximum values are
returned in **a** on the specified image only and the value of **a** on the
other images become undefined. If result_image is not present, the
value is returned on all images. If the execution was successful and
**stat** is present, it is assigned the value zero. If the execution failed,
**stat** gets assigned a nonzero value and, if present, **errmsg** gets assigned
a value describing the occurred error.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**co_min**(3)](CO_MIN),
[**co_sum**(3)](CO_SUM),
[**co_reduce**(3)](CO_REDUCE),
[**co_broadcast**(3)](CO_BROADCAST)

_fortran-lang intrinsic descriptions_

## co_min

### **Name**

**co_min**(3) - \[COLLECTIVE\] Minimal value on the current set of images

### **Syntax**

```fortran
call co_min(a, result_image, stat, errmsg)
```

### **Description**

co_min determines element-wise the minimal value of **a** on all images of
the current team. If result_image is present, the minimal values are
returned in **a** on the specified image only and the value of **a** on the
other images become undefined. If result_image is not present, the
value is returned on all images. If the execution was successful and
**stat** is present, it is assigned the value zero. If the execution failed,
**stat** gets assigned a nonzero value and, if present, **errmsg** gets assigned
a value describing the occurred error.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**co_max**(3)](CO_MAX),
[**co_sum**(3)](CO_SUM),
[**co_reduce**(3)](CO_REDUCE),
[**co_broadcast**(3)](CO_BROADCAST)

_fortran-lang intrinsic descriptions_

## command_argument_count

### **Name**

**command_argument_count**(3) - \[SYSTEM:COMMAND LINE\] Get number of command line arguments

### **Syntax**

```fortran
    result = command_argument_count()

     integer function command_argument_count() result(count)
     integer :: count
```

### **Description**

**command_argument_count()** returns the number of arguments passed
on the command line when the containing program was invoked.

### **Arguments**

None

### **Returns**

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

Fortran 2003 and later

### **See Also**

[**get_command**(3)](GET_COMMAND),
[**get_command_argument**(3)](GET_COMMAND_ARGUMENT)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## compiler_options

### **Name**

**compiler_options**(3) - \[COMPILER INQUIRY\] Options passed to the compiler

### **Syntax**

```fortran
str = compiler_options()
```

### **Description**

compiler_options returns a string with the options used for compiling.

### **Arguments**

None.

### **Returns**

The return value is a default-kind string with system-dependent length.
It contains the compiler flags used to compile the file, which called
the compiler_options intrinsic.

### **Examples**

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
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

[**compiler_version**(3)](COMPILER_VERSION),
**iso_fortran_env**(7)

_fortran-lang intrinsic descriptions_

## compiler_version

### **Name**

**compiler_version**(3) - \[COMPILER INQUIRY\] Compiler version string

### **Syntax**

```fortran
str = compiler_version()
```

### **Description**

**compiler_version**(3) returns a string containing the name and
version of the compiler.

### **Arguments**

None.

### **Returns**

The return value is a default-kind string with system-dependent length.
It contains the name of the compiler and its version number.

### **Examples**

Sample program:

```fortran
program demo_compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
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

[**compiler_options**(3)](COMPILER_OPTIONS),
**iso_fortran_env**(7)

_fortran-lang intrinsic descriptions_

## conjg

### **Name**

**conjg**(3) - \[NUMERIC\] Complex conjugate of a complex value

### **Syntax**

```fortran
z = conjg(z)

   complex(kind=K) elemental function conjg(z)
   complex(kind=K),intent(in) :: z
```

where **K** is the kind of the parameter **z**

### **Description**

**conjg(z)** returns the complex conjugate of the _complex_ value **z**.

In mathematics, the complex conjugate of a complex\_ number is the number
with an equal real part and an imaginary part equal in magnitude but
opposite in sign.

That is, If **z** is **(x, y)** then the result is **(x, -y)**.

For matrices of complex numbers, **conjg(array)** represents the
element-by-element conjugation of **array**; not the conjugate transpose
of **array** .

### **Arguments**

- **z**
  : The type shall be _complex_.

### **Returns**

The return value is of type _complex_.

### **Examples**

Sample program:

```fortran
program demo_conjg
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
complex :: z = (2.0, 3.0)
complex(kind=real64) :: dz = (   &
   &  1.2345678901234567_real64, &
   & -1.2345678901234567_real64)
complex :: arr(3,3)
integer :: i

    print *, z
    z= conjg(z)
    print *, z
    print *

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
 (2.000000,3.000000)
 (2.000000,-3.000000)

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

FORTRAN 77 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## co_reduce

### **Name**

**co_reduce**(3) - \[COLLECTIVE\] Reduction of values on the current set of images

### **Syntax**

```fortran
call co_reduce(a, operation, result_image, stat, errmsg)
```

### **Description**

co_reduce determines element-wise the reduction of the value of **a** on
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

### **Arguments**

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

TS 18508 or later

### **See Also**

[**co_min**(3)](CO_MIN),
[**co_max**(3)](CO_MAX),
[**co_sum**(3)](CO_SUM),
[**co_broadcast**(3)](CO_BROADCAST)

_fortran-lang intrinsic descriptions_

## cosh

### **Name**

**cosh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function

### **Syntax**

```fortran
    result = cosh(x)

     TYPE(kind=KIND) elemental function cosh(x)
     TYPE(kind=KIND),intent(in) :: x
```

where TYPE may be _real_ or _complex_ and KIND may be any
supported kind for the associated type. The returned **value**
will be the same type and kind as the input value **x**.

### **Description**

**cosh(x)** computes the hyperbolic cosine of **x**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

The return value has same type and kind as **x**. If **x** is _complex_, the
imaginary part of the result is in radians.

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

FORTRAN 77 and later, for a complex argument - Fortran 2008 or later

### **See Also**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [**acosh**(3)](ACOSH)

_fortran-lang intrinsic descriptions_

## cos

### **Name**

**cos**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function

### **Syntax**

```fortran
result = cos(x)

   TYPE(kind=KIND),elemental :: cos
   TYPE(kind=KIND,intent(in) :: x
```

where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

### **Description**

**cos(x)** computes the cosine of an angle **x** given the size of the
angle in radians.

The cosine of a _real_ value is the ratio of the adjacent side to the
hypotenuse of a right-angled triangle.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.
  **x** is assumed to be in radians.

### **Returns**

The return value is of the same type and kind as **x**.

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

FORTRAN 77 and later

### **See Also**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[**acos**(3)](ACOS),
[**sin**(3)](SIN),
[**tan**(3)](TAN)

_fortran-lang intrinsic descriptions_

## co_sum

### **Name**

**co_sum**(3) - \[COLLECTIVE\] Sum of values on the current set of images

### **Syntax**

```fortran
call co_sum(a, result_image, stat, errmsg)
```

### **Description**

co_sum sums up the values of each element of **a** on all images of the
current team. If result_image is present, the summed-up values are
returned in **a** on the specified image only and the value of **a** on the
other images become undefined. If result_image is not present, the
value is returned on all images. If the execution was successful and
**stat** is present, it is assigned the value zero. If the execution failed,
**stat** gets assigned a nonzero value and, if present, **errmsg** gets assigned
a value describing the occurred error.

### **Arguments**

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

TS 18508 or later

### **See Also**

[**co_max**(3)](CO_MAX),
[**co_min**(3)](CO_MIN),
[**co_reduce**(3)](CO_REDUCE),
[**co_broadcast**(3)](CO_BROADCAST)

_fortran-lang intrinsic descriptions_

## co_ubound

### **Name**

**co_ubound**(3) - \[COLLECTIVE\] Upper codimension bounds of an array

### **Syntax**

```fortran
result = co_ubound(coarray, dim, kind)
```

### **Description**

Returns the upper cobounds of a coarray, or a single upper cobound along
the **dim** codimension.

### **Arguments**

- **array**
  : Shall be an coarray, of any type.

- **dim**
  : (Optional) Shall be a scalar _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is absent, the
result is an array of the lower cobounds of **coarray**. If **dim** is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

### **Standard**

Fortran 2008 and later

### **See Also**

[**co_lbound**(3)](CO_LBOUND),
[**lbound**(3)](LBOUND),
[**ubound**(3)](UBOUND)

_fortran-lang intrinsic descriptions_

## count

### **Name**

**count**(3) - \[ARRAY REDUCTION\] Count function

### **Syntax**

```fortran
result = count(mask, dim, kind)
```

### **Description**

Counts the number of **.true.** elements in a logical **mask**, or, if the **dim**
argument is supplied, counts the number of elements along each row of
the array in the **dim** direction. If the array has zero size, or all of
the elements of **mask** are false, then the result is **0**.

### **Arguments**

- **mask**
  : The type shall be _logical_.

- **dim**
  : (Optional) The type shall be _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is present, the
result is an array with a rank one less than the rank of **array**, and a
size corresponding to the shape of **array** with the **dim** dimension removed.

### **Examples**

Sample program:

```fortran
program demo_count
implicit none
integer, dimension(2,3) :: a, b
logical, dimension(2,3) :: mymask
      a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
      b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
      print '(3i3)', a(1,:)
      print '(3i3)', a(2,:)
      print *
      print '(3i3)', b(1,:)
      print '(3i3)', b(2,:)
      print *
      mymask = a.ne.b
      print '(3l3)', mymask(1,:)
      print '(3l3)', mymask(2,:)
      print *
      print '(3i3)', count(mymask)
      print *
      print '(3i3)', count(mymask, 1)
      print *
      print '(3i3)', count(mymask, 2)
end program demo_count
```

Expected Results:

```text
  1  3  5
  2  4  6

  0  3  5
  7  4  8

  T  F  F
  T  F  T

  3

  2  0  1

  1  2
```

### **Standard**

Fortran 95 and later, with KIND argument - Fortran 2003
and later

_fortran-lang intrinsic descriptions_

## cpu_time

### **Name**

**cpu_time**(3) - \[SYSTEM:TIME\] return CPU processor time in seconds

### **Syntax**

```fortran
     call cpu_time(time)
     real,intent(out) :: time
```

### **Description**

Returns a _real_ value representing the elapsed CPU time in seconds. This
is useful for testing segments of code to determine execution time.

The exact definition of time is left imprecise because of the
variability in what different processors are able to provide.

If no time source is available, TIME is set to a negative value.

Note that TIME may contain a system dependent, arbitrary offset and may
not start with 0.0. For cpu_time the absolute value is meaningless.
Only differences between subsequent calls, as shown in the example
below, should be used.

A processor for which a single result is inadequate (for example, a
parallel processor) might choose to provide an additional version for
which time is an array.

### **Returns**

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
implicit none
real :: start, finish
   !
   call cpu_time(start)
   ! put code to test here
   call cpu_time(finish)
   !
   ! writes processor time taken by the piece of code.
   print '("Processor Time = ",f6.3," seconds.")',finish-start
end program demo_cpu_time
```

Results:

```text
   Processor Time =  0.000 seconds.
```

### **Standard**

Fortran 95 and later

### **See Also**

[**system_clock**(3)](SYSTEM_CLOCK),
[**date_and_time**(3)](DATE_AND_TIME)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## cshift

### **Name**

**cshift**(3) - \[TRANSFORMATIONAL\] Circular shift elements of an array

### **Syntax**

```fortran
result = cshift(array, shift, dim)
```

### **Description**

**cshift(array, shift \[, dim\])** performs a circular shift on elements
of **array** along the dimension of **dim**. If **dim** is omitted it is taken to be
**1**. **dim** is a scalar of type _integer_ in the range of **1 \<= dim \<= n**,
where "n" is the rank of **array**. If the rank of **array** is one, then all
elements of **array** are shifted by **shift** places. If rank is greater than
one, then all complete rank one sections of **array** along the given
dimension are shifted. Elements shifted out one end of each rank one
section are shifted back in the other end.

### **Arguments**

- **array**
  : Shall be an array of any type.

- **shift**
  : The type shall be _integer_.

- **dim**
  : The type shall be _integer_.

### **Returns**

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

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## c_sizeof

### **Name**

**c_sizeof**(3) - \[ISO_C_BINDING\] Size in bytes of an expression

### **Syntax**

```fortran
n = c_sizeof(x)
```

### **Description**

**c_sizeof(x)** calculates the number of bytes of storage the
expression **x** occupies.

### **Arguments**

- **x**
  : The argument shall be an interoperable data entity.

### **Returns**

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

The example will print .true. unless you are using a platform where
default _real_ variables are unusually padded.

### **Standard**

Fortran 2008

### **See Also**

[**storage_size**(3)](STORAGE_SIZE)

_fortran-lang intrinsic descriptions_

## date_and_time

### **Name**

**date_and_time**(3) - \[SYSTEM:TIME\] gets current time

### **Syntax**

```fortran
    subroutine date_and_time(date, time, zone, values)

     character(len=8),intent(out),optional :: date
     character(len=10),intent(out),optional :: time
     character(len=5),intent(out),optional :: zone
     integer,intent(out),optional :: values(8)
```

### **Description**

**date_and_time(date, time, zone, values)** gets the corresponding
date and time information from the real-time system clock.

Unavailable time and date _character_ parameters return blanks.

### **Arguments**

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

Fortran 95 and later

### **See Also**

[**cpu_time**(3)](CPU_TIME),
[**system_clock**(3)](SYSTEM_CLOCK)

### **Resources**

date and time conversion, formatting and computation

- [M_time](https://github.com/urbanjost/M_time)
- [datetime](https://github.com/wavebitscientific/datetime-fortran)
- [datetime-fortran](https://github.com/wavebitscientific/datetime-fortran)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## dble

### **Name**

**dble**(3) - \[TYPE:NUMERIC\] Double conversion function

### **Syntax**
```fortran
result = dble(a)

    elemental function dble(a)
    type(real(kind=kind(0.0d0)))     :: dble
    type(TYPE(kind=KIND)),intent(in) :: a
```
where TYPE may be _integer_, _real_, or _complex_ and KIND any kind
supported by the TYPE.

### **Description**

**dble(a)** Converts **a** to double precision _real_ type.

### **Arguments**

- **a**
  : The type shall be _integer_, _real_, or _complex_.

### **Returns**

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

FORTRAN 77 and later

### **See Also**

[**real**(3)](REAL),
[**cmplx**(3)](CMPLX),
[**aimag**(3)](AIMAG),
[**int**(3)](INT)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## digits

### **Name**

**digits**(3) - \[NUMERIC MODEL\] Significant digits function

### **Syntax**

```fortran
result = digits(x)
    function digits(x)
    type(integer(kind=kind(0)))      :: digits
    type(TYPE(kind=KIND)),intent(in) :: x(..)
```

where TYPE may be _integer_ or _real_ and KIND is any kind supported by
TYPE.

### **Description**

**digits(x)** returns the number of significant digits of the internal
model representation of **x**. For example, on a system using a 32-bit
floating point representation, a default real number would likely return 24.

### **Arguments**

- **x**
  : The type may be a scalar or array of type _integer_ or _real_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## dim

### **Name**

**dim**(3) - \[NUMERIC\] Positive difference

### **Syntax**

```fortran
result = dim(x, y)

    elemental function dim(x, y)
    type(TYPE(kind=KIND))            :: dim
    type(TYPE(kind=KIND)),intent(in) :: x, y
```

where TYPE may be _real_ or _integer_ and KIND is any supported kind for the type.

### **Description**

**dim(x,y)** returns the difference **x - y** if the result is positive;
otherwise it returns zero.

### **Arguments**

- **x**
  : The type shall be _integer_ or _real_

- **y**
  : The type shall be the same type and kind as **x**.

### **Returns**

The return value is the same type and kind as the input arguments **x** and **y**.

### **Examples**

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer           :: i
real(kind=real64) :: x
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

FORTRAN 77 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## dot_product

### **Name**

**dot_product**(3) - \[TRANSFORMATIONAL\] Dot product function

### **Syntax**

```fortran
result = dot_product(vector_a, vector_b)
```

### **Description**

**dot_product(vector_a, vector_b)** computes the dot product
multiplication of two vectors vector*a and vector_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are \_integer* or _real_, the result is
**sum(vector_a\*vector_b)**. If the vectors are _complex_, the result is
**sum(conjg(vector_a)\*vector_b)**. If the vectors are _logical_, the
result is **any(vector_a .and. vector_b)**.

### **Arguments**

- **vector_a**
  : The type shall be numeric or _logical_, rank 1.

- **vector_b**
  : The type shall be numeric if vector*a is of numeric type or \_logical*
  if vector*a is of type \_logical*. vector_b shall be a rank-one
  array.

### **Returns**

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is .true. or .false..

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

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## dprod

### **Name**

**dprod**(3) - \[NUMERIC\] Double product function

### **Syntax**

```fortran
result = dprod(x, y)
```

### **Description**

**dprod(x,y)** produces a higher _doubleprecision_ product of default _real_
numbers **x** and **y**.

The result has a value equal to a processor-dependent approximation to
the product of **x** and **y**. It is recommended that the processor compute the
product in double precision, rather than in single precision and then
converted to double precision.

- **x**
  : shall be default real.

- **y**
  : shall be default real.

The setting of compiler options specifying _real_ size can affect this
function.

### **Arguments**

- **x**
  : Must be of default _real(kind=kind(0.0))_ type

- **y**
  : Must have the same type and kind parameters as **x**

### **Returns**

The return value is of type _real(kind=kind(0.0d0))_.

### **Examples**

Sample program:

```fortran
program demo_dprod
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
integer,parameter :: dp=kind(0.0d0)
real :: x = 5.2
real :: y = 2.3
real(kind=dp) :: dd
   dd = dprod(x,y)
   print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))
   ! interesting comparisons
   print *, 52*23
   print *, 52*23/100.0
   print *, 52*23/100.0d0

   !! common extension is to take doubleprecision arguments
   !! and return higher precision
   bigger: block
   doubleprecision :: xx = 5.2d0
   doubleprecision :: yy = 2.3d0
   real(kind=real128) :: ddd
   !ddd = dprod(xx,yy)
   !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))
   endblock bigger

end program demo_dprod
```

Results:

```text
   11.959999313354501 11.9599991 4 8 8
        1196
   11.9600000
   11.960000000000001
```

### **Standard**

FORTRAN 77 and later

_fortran-lang intrinsic descriptions_

## dshiftl

### **Name**

**dshiftl**(3) - \[BIT:COPY\] combines bits of arguments **i** and **j**

### **Syntax**

```fortran
result = dshiftl(i, j, shift)
```

### **Description**

**dshiftl(i, j, shift)** combines bits of **i** and **j**. The rightmost **shift**
bits of the result are the leftmost **shift** bits of **j**, and the remaining
bits are the rightmost bits of **i**.

### **Arguments**

- **i**
  : Shall be of type _integer_.

- **j**
  : Shall be of type _integer_, and of the same kind as **i**.

- **shift**
  : Shall be of type _integer_.

### **Returns**

The return value has same type and kind as **i**.

### **Standard**

Fortran 2008 and later

### **See Also**

[**dshiftr**(3)](DSHIFTR)

_fortran-lang intrinsic descriptions_

## dshiftr

### **Name**

**dshiftr**(3) - \[BIT:COPY\] combines bits of arguments **i** and **j**

### **Syntax**

```fortran
result = dshiftr(i, j, shift)
```

### **Description**

**dshiftr(i, j, shift)** combines bits of **i** and **j**. The leftmost **shift**
bits of the result are the rightmost **shift** bits of **i**, and the remaining
bits are the leftmost bits of **j**.

### **Arguments**

- **i**
  : Shall be of type _integer_.

- **j**
  : Shall be of type _integer_, and of the same kind as **i**.

- **shift**
  : Shall be of type _integer_.

### **Returns**

The return value has same type and kind as **i**.

### **Standard**

Fortran 2008 and later

### **See Also**

[**dshiftl**(3)](DSHIFTL)

_fortran-lang intrinsic descriptions_

## eoshift

### **Name**

**eoshift**(3) - \[TRANSFORMATIONAL\] End-off shift elements of an array

### **Syntax**

```fortran
result = eoshift(array, shift, boundary, dim)
```

### **Description**

**eoshift(array, shift\[, boundary, dim\])** performs an end-off shift
on elements of **array** along the dimension of **dim**. If **dim** is omitted it is
taken to be **1**. **dim** is a scalar of type _integer_ in the range of **1 \<= DIM
\<= n** where **"n"** is the rank of **array**. If the rank of **array** is one, then
all elements of **array** are shifted by **shift** places. If rank is greater
than one, then all complete rank one sections of **array** along the given
dimension are shifted. Elements shifted out one end of each rank one
section are dropped. If **boundary** is present then the corresponding value
from **boundary** is copied back in the other end. If **boundary** is not
present then the following are copied in depending on the type of **array**.

    Array Type     | Boundary Value
    -----------------------------------------------------
    Numeric        | 0 of the type and kind of **array**
    Logical        | .false.
    Character(len) |  LEN blanks

### **Arguments**

- **array**
  : May be any type, not scalar.

- **shift**
  : The type shall be _integer_.

- **boundary**
  : Same type as ARRAY.

- **dim**
  : The type shall be _integer_.

### **Returns**

Returns an array of same type and rank as the **array** argument.

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

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## epsilon

### **Name**

**epsilon**(3) - \[NUMERIC MODEL\] Epsilon function

### **Syntax**

```fortran
result = epsilon(x)
```
### **Description**

**epsilon(x)** returns the floating point relative accuracy.
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

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## erfc

### **Name**

**erfc**(3) - \[MATHEMATICS\] Complementary error function

### **Syntax**

```fortran
result = erfc(x)

   elemental function erfc(x)
   real(kind=KIND) :: erfc
   real(kind=KIND),intent(in) :: x
```

### **Description**

**erfc**(x) computes the complementary error function of **x**. Simply put
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

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

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

Fortran 2008 and later

### See also

[**erf**(3)](ERF)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## erfc_scaled

### **Name**

**erfc_scaled**(3) - \[MATHEMATICS\] Error function

### **Syntax**

```fortran
result = erfc_scaled(x)
```

### **Description**

**erfc_scaled**(x) computes the exponentially-scaled complementary
error function of **x**:

$$
e^{x^2} \frac{2}{\sqrt{\pi}} \int_{x}^{\infty}
e^{-t^2} dt.
$$

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

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

Fortran 2008 and later

_fortran-lang intrinsic descriptions_

## erf

### **Name**

**erf**(3) - \[MATHEMATICS\] Error function

### **Syntax**

```fortran
result = erf(x)
```

### **Description**

**erf**(x) computes the error function of **x**, defined as

$$
\text{erf}(x) = \frac{2}{\sqrt{\pi}} \int_0^x e^{__-t__^2} dt.
$$

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

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

Fortran 2008 and later

### See also

[**erfc**(3)](ERFC)

- [Wikipedia:error function](https://en.wikipedia.org/wiki/Error_function)

_fortran-lang intrinsic descriptions_

## event_query

### **Name**

**event_query**(3) - \[COLLECTIVE\] Query whether a coarray event has occurred

### **Syntax**

```fortran
call event_query(event, count, stat)
```

### **Description**

**event_query** assigns the number of events to **count** which have been
posted to the **event** variable and not yet been removed by calling
**event_wait**. When **stat** is present and the invocation was successful, it
is assigned the value **0**. If it is present and the invocation has failed,
it is assigned a positive value and **count** is assigned the value **-1**.

### **Arguments**

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

TS 18508 or later

_fortran-lang intrinsic descriptions_

## execute_command_line

### **Name**

**execute_command_line**(3) - \[SYSTEM:PROCESSES\] Execute a shell command

### **Syntax**

```fortran
   subroutine execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)

    character(len=*),intent(in)  :: command
    logical,intent(in),optional  :: wait
    integer,intent(out),optional :: exitstat
    integer,intent(out),optional :: cmdstat
    character(len=*),intent(out),optional :: cmdmsg
```

### **Description**

The **command** argument is passed to the shell and executed. (The shell is
generally **sh**(1) on Unix systems, and cmd.exe on Windows.) If **wait** is
present and has the value **.false.**, the execution of the command is
asynchronous if the system supports it; otherwise, the command is
executed synchronously.

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

### **Arguments**

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

Fortran 2008 and later

_fortran-lang intrinsic descriptions_

## exp

### **Name**

**exp**(3) - \[MATHEMATICS\] Exponential function

### **Syntax**

```fortran
result = exp(x)
```

### **Description**

**exp**(x) computes the base "_e_" exponential of **x** where "_e_" is
_Euler's constant_.

If **x** is of type _complex_, its imaginary part is regarded as a value
in radians such that (see _Euler's formula_):

if
**cx=(re,im)**
then
**exp(cx)=exp(re)\*cmplx(cos(im),sin(im),kind=kind(cx))**

Since **exp**(3) is the inverse function of **log**(3) the maximum valid magnitude
of the _real_ component of **x** is **log(huge(x))**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

The value of the result is **e\*\*x** where **e** is Euler's constant.

The return value has the same type and kind as **x**.

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

FORTRAN 77 and later

### **See Also**

- [**log**(3)](LOG)

- Wikipedia:[Exponential function](https://en.wikipedia.org/wiki/Exponential_function)

- Wikipedia:[Euler's formula](https://en.wikipedia.org/wiki/Euler%27s_formula)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## exponent

### **Name**

**exponent**(3) - \[MODEL_COMPONENTS\] Exponent function

### **Syntax**

```fortran
result = exponent(x)
```

### **Description**

**exponent**(x) returns the value of the exponent part of **x**. If **x** is
zero the value returned is zero.

### **Arguments**

- **x**
  : The type shall be _real_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## extends_type_of

### **Name**

**extends_type_of**(3) - \[STATE\] determine if the dynamic type of **a** is an extension of the dynamic type of **mold**.

### **Syntax**

```fortran
result=extends_type_of(a, mold)
```

### **Description**

**extends_type_of**(3) is **.true.** if and only if the dynamic type of **a**
is an extension of the dynamic type of **mold**.

### **Options**

- **a**
  : shall be an object of extensible type. If it is a pointer, it
  shall not have an undefined association status.

- **mold**
  : shall be an object of extensible type. If it is a pointer, it
  shall not have an undefined association status.

### **Returns**

- **result**
  : Default logical scalar.

- **value**
  : If **mold** is unlimited polymorphic and is either a disassociated
  pointer or unallocated allocatable variable, the result is
  true; otherwise if **a** is unlimited polymorphic and is either a
  disassociated pointer or unallocated allocatable variable, the result
  is false; otherwise the result is true if and only if the dynamic
  type of **a** is an extension type of the dynamic type of **mold**.

  The dynamic type of a disassociated pointer or unallocated
  allocatable variable is its declared type.

### **Examples**

_fortran-lang intrinsic descriptions_

## findloc

### **Name**

**findloc**(3) - \[ARRAY:LOCATION\] Location of first element of ARRAY identified by MASK along dimension DIM having a value

### **Syntax**

```fortran
findloc (array, value, dim, mask, kind, back)

or

findloc(array, value, mask, kind, back)
```

### **Description**

Location of the first element of **array** identified by **mask** along
dimension **dim** having a value equal to **value**.

If both **array** and **value** are of type logical, the comparison is
performed with the **.eqv.** operator; otherwise, the comparison is
performed with the == operator. If the value of the comparison is
true, that element of **array** matches **value**.

If only one element matches **value**, that element's subscripts are
returned. Otherwise, if more than one element matches **value** and
**back** is absent or present with the value false, the element whose
subscripts are returned is the first such element, taken in array
element order. If **back** is present with the value true, the element
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

### **Returns**

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

### **Returns**

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

**floor**(3) - \[NUMERIC\] function to return largest integral value not greater than argument

### **Syntax**

```fortran
result = floor(a, KIND)

    elemental function floor(a,KIND)
    integer(kind=KIND) :: floor
    real(kind=kind(a)),intent(in) :: a
    integer(kind=IKIND),intent(in),optional :: KIND
```
where _KIND_ is any valid value for type _integer_.

### **Description**

**floor(a)** returns the greatest integer less than or equal to **a**.
That is, it picks the whole number at or to the left of the value on
the scale **-huge(int(a,kind=KIND))-1** to **huge(int(a),kind=KIND)**.

### **Arguments**

- **a**
  : The type shall be _real_.

- **kind**
  : (Optional) A scalar _integer_ constant initialization expression
  indicating the kind parameter of the result.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**ceiling**(3)](CEILING),
[**nint**(3)](NINT)

[**aint**(3)](AINT),
[**anint**(3)](ANINT),
[**int**(3)](INT),
[**selected_int_kind**(3)](SELECTED_INT_KIND)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## fraction

### **Name**

**fraction**(3) - \[MODEL_COMPONENTS\] Fractional part of the model representation

### **Syntax**

```fortran
y = fraction(x)
```

### **Description**

**fraction(x)** returns the fractional part of the model representation
of **x**.

### **Arguments**

- **x**
  : The type of the argument shall be a _real_.

### **Returns**

The return value is of the same type and kind as the argument. The
fractional part of the model representation of **x** is returned; it is
**x \* radix(x)\*\*(-exponent(x))**.

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## gamma

### **Name**

**gamma**(3) - \[MATHEMATICS\] Gamma function, which yields factorials for positive whole numbers

### **Syntax**

```fortran
x = gamma(x)
```

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

### **Arguments**

- **x**
  : Shall be of type _real_ and neither zero nor a negative integer.

### **Returns**

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

Fortran 2008 and later

### **See Also**

Logarithm of the Gamma function: [**log_gamma**(3)](LOG_GAMMA)

[Wikipedia: Gamma_function](https://en.wikipedia.org/wiki/Gamma_function)

_fortran-lang intrinsic descriptions_

## get_command_argument

### **Name**

**get_command_argument**(3) - \[SYSTEM:COMMAND LINE\] Get command line arguments

### **Syntax**

```fortran
     call get_command_argument(number, value, length, status)

     subroutine get_command_argument(number,value,length.status)
     integer,intent(in)                    :: number
     character(len=*),intent(out),optional :: value
     integer,intent(out),optional          :: length
     integer,intent(out),optional          :: status
```

### **Description**

Retrieve the **number**-th argument that was passed on the command line
when the containing program was invoked.

There is not anything specifically stated about what an argument is but
in practice the arguments are split on whitespace unless the arguments
are quoted and IFS values (Internal Field Separators) used by common
shells are ignored.

### **Options**

- **number**
  : Shall be a scalar of type **integer**, **number \>= 0**. If **number =
  0**, **value** is set to the name of the program (on systems that support
  this feature).

### **Returns**

- **value**
  : Shall be a scalar of type _character_ and of default kind. After
  get_command_argument returns, the **value** argument holds the
  **number**-th command line argument. If **value** can not hold the argument,
  it is truncated to fit the length of **value**. If there are less than
  **number** arguments specified at the command line, **value** will be filled
  with blanks.

- **length**
  : (Optional) Shall be a scalar of type _integer_. The **length**
  argument contains the length of the **number**-th command line argument.

- **status**
  : (Optional) Shall be a scalar of type _integer_. If the argument
  retrieval fails, **status** is a positive number; if **value** contains a
  truncated command line argument, **status** is **-1**; and otherwise the
  **status** is zero.

### **Examples**

Sample program:

```fortran
program demo_get_command_argument
implicit none
character(len=255)           :: progname
integer                      :: stat
integer                      :: count,i, longest, argument_length
integer,allocatable          :: istat(:), ilen(:)
character(len=:),allocatable :: args(:)
  !
  ! get number of arguments
  count = command_argument_count()
  write(*,*)'The number of arguments is ',count
  !
  ! simple usage
  !
  call get_command_argument (0, progname, status=stat)
  if (stat == 0) then
     print *, "The program's name is " // trim (progname)
  endif
  !
  ! showing how to make an array to hold any argument list
  !
  ! find longest argument
  !
  longest=0
  do i=0,count
     call get_command_argument(number=i,length=argument_length)
     longest=max(longest,argument_length)
  enddo
  !
  ! allocate string array big enough to hold command line
  ! argument strings and related information
  !
  allocate(character(len=longest) :: args(0:count))
  allocate(istat(0:count))
  allocate(ilen(0:count))
  !
  ! read the arguments into the array
  !
  do i=0,count
    call get_command_argument(i, args(i),status=istat(i),length=ilen(i))
  enddo
  !
  ! show the results
  !
  write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
  & (i,istat(i),ilen(i),args(i)(:ilen(i)),i=0,count)
end program demo_get_command_argument
```

Results:

```text
/demo_get_command_argument a    test  'of getting   arguments  ' "  leading"

 The number of arguments is            5
 The program's name is xxx
000 00000 00003 [./test_get_command_argument]
001 00000 00001 [a]
003 00000 00004 [test]
004 00000 00024 [of getting   arguments  ]
005 00000 00018 [  leading]
```

### **Standard**

Fortran 2003 and later

### **See Also**

[**get_command**(3)](GET_COMMAND),
[**command_argument_count**(3)](COMMAND_ARGUMENT_COUNT)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## get_command

### **Name**

**get_command**(3) - \[SYSTEM:COMMAND LINE\] Get the entire command line

### **Syntax**

```fortran
   call get_command(command, length, status)

    subroutine get_command(command,length,status)
    character(len=*),intent(out),optional :: command
    integer,intent(out),optional :: length
    integer,intent(out),optional :: status
```

### **Description**

Retrieve the entire command line that was used to invoke the program.

Note that what is typed on the command line is often processed by
a shell. The shell typically processes special characters and white
space before passing it to the program. The processing can typically be
turned off by turning off globbing or quoting the command line arguments
and/or changing the default field separators, but this should rarely
be necessary.

### **Returns**

- **command**
  : Shall be of type _character_ and of default kind. If
  **command** is present, stores the entire command line that was used to
  invoke the program in **command**.

- **length**
  : Shall be of type _integer_ and of default kind. If **length**
  is present, it is assigned the length of the command line.

- **status**
  : Shall be of type _integer_ and of default kind. If **status**
  is present, it is assigned 0 upon success of the command, **-1** if
  **command** is too short to store the command line, or a positive value
  in case of an error.

### **Examples**

Sample program:

```fortran
program demo_get_command
implicit none
integer                      :: COMMAND_LINE_LENGTH
character(len=:),allocatable :: COMMAND_LINE
   ! get command line length
   call get_command(length=COMMAND_LINE_LENGTH)
   ! allocate string big enough to hold command line
   allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)
   ! get command line as a string
   call get_command(command=COMMAND_LINE)
   ! trim leading spaces just in case
   COMMAND_LINE=adjustl(COMMAND_LINE)
   write(*,'("OUTPUT:",a)')COMMAND_LINE
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

Fortran 2003 and later

### **See Also**

[**get_command_argument**(3)](GET_COMMAND_ARGUMENT),
[**command_argument_count**(3)](COMMAND_ARGUMENT_COUNT)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## get_environment_variable

### **Name**

**get_environment_variable**(3) - \[SYSTEM:ENVIRONMENT\] Get an environmental variable

### **Syntax**

```fortran
  call get_environment_variable(name, value, length, status, trim_name)

   character(len=*),intent(in) :: name
   character(len=*),intent(out),optional :: value
   integer,intent(out),optional :: length
   integer,intent(out),optional :: status
   logical,intent(out),optional :: trim_name
```

### **Description**

Get the **value** of the environmental variable **name**.

Note that **get_environment_variable**(3) need not be thread-safe. It
is the responsibility of the user to ensure that the environment is not
being updated concurrently.

### **Options**

- **name**
  : The name of the environment variable to query.

    Shall be a scalar of type _character_ and of default kind.

### **Returns**

- **value**
  : The value of the environment variable being queried.

  Shall be a scalar of type _character_ and of default kind.
  The value of **name** is stored in **value**. If **value** is not
  large enough to hold the data, it is truncated. If **name** is not
  set, **value** will be filled with blanks.

- **length**
  : Argument **length** contains the length needed for storing the
  environment variable **name** or zero if it is not present.

  Shall be a scalar of type _integer_ and of default kind.

- **status**
  : **status** is **-1** if **value** is present but too short for the
  environment variable; it is **1** if the environment variable does
  not exist and **2** if the processor does not support environment
  variables; in all other cases **status** is zero.

  Shall be a scalar of type _integer_ and of default kind.

- **trim_name**
  : If **trim_name** is present with the value **.false.**, the trailing
  blanks in **name** are significant; otherwise they are not part of the
  environment variable name.

  Shall be a scalar of type _logical_ and of default kind.

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

function get_env(NAME,DEFAULT) result(VALUE)
! a function that makes calling get_environment_variable(3) simple
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   VALUE=''
   if(NAME.ne.'')then
      call get_environment_variable( &
      & NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
       !*!print *, NAME, " is not defined in the environment. Strange..."
       VALUE=''
      case (2)
       !*!print *, &
       !*!"This processor does not support environment variables. Boooh!"
       VALUE=''
      case default
       ! make string to hold value of sufficient size
       if(allocated(VALUE))deallocate(VALUE)
       allocate(character(len=max(howbig,1)) :: VALUE)
       ! get value
       call get_environment_variable( &
       & NAME,VALUE,status=stat,trim_name=.true.)
       if(stat.ne.0)VALUE=''
      end select
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env

end program demo_getenv
```

Typical Results:

```text
   HOME="/home/urbanjs"
```

### **Standard**

Fortran 2003 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## huge

### **Name**

**huge**(3) - \[NUMERIC MODEL\] Largest number of a type and kind

### **Syntax**

```fortran
result = huge(x)

   function huge(x) result(answer)
   TYPE(kind=KIND),intent(in) :: x
   TYPE(kind=KIND) :: answer
```

where **TYPE** may be _real_ or _integer_ and **KIND** is any supported
associated _kind_.

### **Description**

**huge(x)** returns the largest number that is not an infinity for the
kind and type of **x**.

### **Arguments**

- **x**
  : Shall be an arbitrary value of type _real_ or _integer_.
  The value is used merely to determine what _kind_ and _type_ of
  scalar is being queried.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## hypot

### **Name**

**hypot**(3) - \[MATHEMATICS\] returns the distance between the point and the origin.

### **Syntax**

```fortran
result = hypot(x, y)

   real(kind=KIND) elemental function hypot(x,y) result(value)
   real(kind=KIND),intent(in) :: x, y
```

where **x,y,value** shall all be of the same **kind**.

### **Description**

**hypot(x,y)** is referred to as the Euclidean distance function. It is
equal to $\sqrt{x^2+y^2}$, without undue underflow or overflow.

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.

**hypot(x,y)** returns the distance between the point **<x,y>** and the origin.

### **Arguments**

- **x**
  : The type shall be _real_.

- **y**
  : The type and kind type parameter shall be the same as **x**.

### **Returns**

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

Fortran 2008 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## iachar

### **Name**

**iachar**(3) - \[CHARACTER:CONVERSION\] Code in ASCII collating sequence

### **Syntax**

```fortran
result = iachar(c, kind)
```

### **Description**

**iachar**(c) returns the code for the ASCII character in the first
character position of C.

### **Arguments**

- **c**
  : Shall be a scalar _character_, with _intent(in)_

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind.

### **Examples**

Sample program:

```fortran
program demo_iachar
implicit none
! create function to convert uppercase letters to lowercase
   write(*,'(a)')lower('abcdefg ABCDEFG')
contains
!
elemental pure function lower(str) result (string)
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
   abcdefg abcdefg
```

### **Note**

See [**ichar**(3)](ICHAR) for a discussion of converting between numerical
values and formatted string representations.

### **Standard**

Fortran 95 and later, with KIND argument - Fortran 2003 and later

### **See Also**

[**achar**(3)](ACHAR),
[**char**(3)](CHAR),
[**ichar**(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL), [**adjustr**(3)](ADJUSTR), [**index**(3)](INDEX),
  [**scan**(3)](SCAN), [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT), [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## iall

### **Name**

**iall**(3) - \[BIT:LOGICAL\] Bitwise and of array elements

### **Syntax**

```fortran
  result = iall(array, mask)

    or

  result = iall(array, dim, mask)
```

### **Description**

Reduces with bitwise _and_ the elements of **array** along dimension **dim** if
the corresponding element in **mask** is **.true.**.

### **Arguments**

- **array**
  : Shall be an array of type _integer_

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from **1 to n**, where **n** equals the rank of **array**.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Returns**

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

Fortran 2008 and later

### **See Also**

[**iany**(3)](IANY),
[**iparity**(3)](IPARITY),
[**iand**(3)](IAND)

_fortran-lang intrinsic descriptions_

## iand

### **Name**

**iand**(3) - \[BIT:LOGICAL\] Bitwise logical and

### **Syntax**

```fortran
result = iand(i, j)
```

### **Description**

Bitwise logical **and**.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **j**
  : The type shall be _integer_, of the same kind as **i**.

### **Returns**

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### **Examples**

Sample program:

```fortran
program demo_iand
implicit none
integer :: a, b
      data a / z'f' /, b / z'3' /
      write (*,*) iand(a, b)
end program demo_iand
```

Results:

```text
              3
```

### **Standard**

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**not**(3)](NOT),
[**mvbits**(3)](MVBITS)

_fortran-lang intrinsic descriptions_

## iany

### **Name**

**iany**(3) - \[BIT:LOGICAL\] Bitwise or of array elements

### **Syntax**

```fortran
  result = iany(array, mask)

    or

  result = iany(array, dim, mask)
```

### **Description**

Reduces with bitwise or (inclusive or) the elements of **array** along
dimension **dim** if the corresponding element in **mask** is **.true.**.

### **Arguments**

- **array**
  : Shall be an array of type _integer_

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from **1 to n**, where **n** equals the rank of **array**.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Returns**

The result is of the same type as **array**.

If **dim** is absent, a scalar with the bitwise _or_ of all elements in **array**
is returned. Otherwise, an array of rank **n-1**, where **n** equals the
rank of **array**, and a shape similar to that of **array** with dimension **dim**
dropped is returned.

### **Examples**

Sample program:

```fortran
program demo_iany
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
integer(kind=int8) :: a(2)
     a(1) = int(b'00100100')
     a(2) = int(b'01101010')
     print '(b8.8)', iany(a)
end program demo_iany
```

Results:

```
   01101110
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**iparity**(3)](IPARITY),
[**iall**(3)](IALL),
[**ior**(3)](IOR)

_fortran-lang intrinsic descriptions_

## ibclr

### **Name**

**ibclr**(3) - \[BIT:SET\] Clear bit

### **Syntax**

```fortran
result = ibclr(i, pos)
```

### **Description**

**ibclr** returns the value of **i** with the bit at position **pos** set to zero.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit. **pos** is an **intent(in)** scalar or array of type
  _integer_. The value of **pos** must be within the range zero to
  **(bit_size(i)-1**).

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**mvbits**(3)](MVBITS),
[**not**(3)](NOT)

_fortran-lang intrinsic descriptions_

## ibits

### **Name**

**ibits**(3) - \[BIT:COPY\] Bit extraction

### **Syntax**

```fortran
result = ibits(i, pos, len)
```

### **Description**

**ibits** extracts a field of length **len** from **i**, starting from
bit position **pos** and extending left for **len** bits. The result is
right-justified and the remaining bits are zeroed. The value of pos+len
must be less than or equal to the value **bit_size(i)**.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit.

- **len**
  : The type shall be _integer_.

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**not**(3)](NOT),
[**mvbits**(3)](MVBITS)

_fortran-lang intrinsic descriptions_

## ibset

### **Name**

**ibset**(3) - \[BIT:SET\] Set bit

### **Syntax**

```fortran
result = ibset(i, pos)
```

### **Description**

**ibset** returns the value of **i** with the bit at position **pos** set to one.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit. pos is an **intent(in)** scalar or array of type
  _integer_. The value of pos must be within the range zero to
  **(bit_size(i)-1**).

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**not**(3)](NOT),
[**mvbits**(3)](MVBITS)

_fortran-lang intrinsic descriptions_

## ichar

### **Name**

**ichar**(3) - \[CHARACTER:CONVERSION\] Character-to-integer conversion function

### **Syntax**

```fortran
   elemental function ichar(c,kind)

    character(len=1),intent(in) :: c
    integer,intent(in),optional :: kind
```

### **Description**

**ichar(c)** returns the code for the character in the system's native
character set. The correspondence between characters and their codes is
not necessarily the same across different Fortran implementations. For
example, a platform using EBCDIC would return different values than an
ASCII platform.

See **iachar**(3) for specifically working with the ASCII character
set.

### **Arguments**

- **c**
  : Shall be a scalar _character_, with **intent(in)**

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default _integer_ kind.

### **Examples**

Sample program:

```fortran
program demo_ichar
implicit none
integer i

   write(*,*)ichar(['a','z','A','Z'])
   do i=0,127
      call printme()
   enddo

contains

   subroutine printme()
   character(len=1) :: letter

      letter=char(i)
      select case(i)
      case (:31,127:)
         write(*,'(1x,i0.3,1x,"HEX=",z2.2,1x,i0)')i,letter,ichar(letter)
      case default
         write(*,'(1x,i0.3,1x,a,1x,i0)')i,letter,ichar(letter)
      end select

   end subroutine printme

end program demo_ichar
```

### **Note**

No intrinsic exists to convert between a numeric value and a formatted
character string representation -- for instance, given the _character_
value '154', obtaining an _integer_ or _real_ value with the value 154, or
vice versa. Instead, this functionality is provided by internal-file
I/O, as in the following example:

```
program read_val
integer value
character(len=10) string, string2
   string = '154'

   ! Convert a string to a numeric value
   read (string,'(I10)') value
   print *, value

   ! Convert a value to a formatted string
   write (string2,'(I10)') value
   print *, string2
end program read_val
```

Results:

```text
            154
           154
```

### **Standard**

Fortran 95 and later, with KIND argument -Fortran 2003 and later

### **See Also**

[**achar**(3)](ACHAR),
[**char**(3)](CHAR),
[**iachar**(3)](IACHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),

[**scan**(3)](SCAN),
[**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## ieor

### **Name**

**ieor**(3) - \[BIT:LOGICAL\] Bitwise logical exclusive or

### **Syntax**

```fortran
result = ieor(i, j)
```

### **Description**

**ieor** returns the bitwise Boolean exclusive-**or** of **i** and **j**.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **j**
  : The type shall be _integer_, of the same kind as **i**.

### **Returns**

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### **Standard**

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**not**(3)](NOT),
[**mvbits**(3)](MVBITS)

_fortran-lang intrinsic descriptions_

## image_index

### **Name**

**image_index**(3) - \[COLLECTIVE\] Cosubscript to image index conversion

### **Syntax**

```fortran
result = image_index(coarray, sub)
```

### **Description**

Returns the image index belonging to a cosubscript.

### **Arguments**

- **coarray**
  : Coarray of any type.

- **sub**
  : default integer rank-1 array of a size equal to the corank of
  **coarray**.

### **Returns**

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

Fortran 2008 and later

### **See Also**

[**this_image**(3)](THIS_IMAGE),
[**num_images**(3)](NUM_IMAGES)

_fortran-lang intrinsic descriptions_

## index

### **Name**

**index**(3) - \[CHARACTER:SEARCH\] Position of a substring within a string

### **Syntax**

```fortran
   index(string, substring, back, kind) result(start)

     character(len=*),intent(in) :: string
     character(len=*),intent(in) :: substring
     logical,intent(in),optional :: back
     integer,intent(in),optional :: kind
     integer(kind=KIND)          :: start
```

### **Description**

Returns the position of the start of the leftmost or rightmost
occurrence of string **substring** in **string**, counting from one. If
**substring** is not present in **string**, zero is returned.

### **Arguments**

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

### **Returns**

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

FORTRAN 77 and later, with KIND argument Fortran 2003
and later

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL), [**adjustr**(3)](ADJUSTR), [**index**(3)](INDEX),
  [**scan**(3)](SCAN), [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT), [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## int

### **Name**

**int**(3) - \[TYPE:NUMERIC\] Convert to integer type by truncating towards zero

### **Syntax**

```fortran
result = int(a, kind)

 integer(kind=KIND) elemental function int(a,kind)
 TYPE(kind=KIND),intent(in),optional :: a
 integer,optional :: kind
```

### **Description**

Convert to integer type by truncating towards zero.

### **Arguments**

- **a**
  : Shall be of type _integer_, _real_, or _complex_ or a BOZ-literal-constant.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

  If not present the returned type is that of default integer type.

### **Returns**

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

FORTRAN 77 and later

### **See Also**

[**aint**(3)](AINT),
[**anint**(3)](ANINT),
[**nint**(3)](NINT),
[**selected_int_kind**(3)](SELECTED_INT_KIND),
[**ceiling**(3)](CEILING),
[**floor**(3)](FLOOR)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## ior

### **Name**

**ior**(3) - \[BIT:LOGICAL\] Bitwise logical inclusive or

### **Syntax**

```fortran
   result = ior(i, j)
    integer,intent(in) :: i
    integer,intent(in) :: j
```

### **Description**

**ior** returns the bit-wise Boolean inclusive-**or** of **i** and **j**.

### **Arguments**

- **i**
  : an _integer_ scalar or array.

- **j**
  : _integer_ scalar or array, of the same kind as **i**.

### **Returns**

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

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

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**not**(3)](NOT),
[**mvbits**(3)](MVBITS)

_fortran-lang intrinsic descriptions_

## iparity

### **Name**

**iparity**(3) - \[BIT:LOGICAL\] Bitwise exclusive or of array elements

### **Syntax**

```fortran
  result = iparity(array, mask)

   or

  result = iparity(array, dim, mask)
```

### **Description**

Reduces with bitwise _xor_ (exclusive _or_) the elements of **array** along
dimension **dim** if the corresponding element in **mask** is **.true.**.

### **Arguments**

- **array**
  : Shall be an array of type _integer_

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from **"1" to "n"**, where **"n"** equals the rank of **array**.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Returns**

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

Fortran 2008 and later

### **See Also**

[**iany**(3)](IANY),
[**iall**(3)](IALL),
[**ieor**(3)](IEOR),
[**parity**(3)](PARITY)

_fortran-lang intrinsic descriptions_

## is_contiguous

### **Name**

**is_contiguous**(3) - \[ARRAY INQUIRY\] test if object is contiguous

### **Syntax**

```fortran
result = is_contiguous(a)
```

### **Description**

True if and only if an object is contiguous.

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

### **Arguments**

- **a**
  : may be of any type. It shall be an array. If it is a pointer it
  shall be associated.

### **Returns**

- **Result**
  : of type Default logical scalar. The result has the value true if **a**
  is contiguous, and false otherwise.

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

Fortran 2008 and later

_fortran-lang intrinsic descriptions_

## ishftc

### **Name**

**ishftc**(3) - \[BIT:SHIFT\] Shift bits circularly

### **Syntax**

```fortran
result = ishftc(i, shift, size)
```

### **Description**

**ishftc**(3) returns a value corresponding to **i** with the rightmost **size** bits
shifted circularly **shift** places; that is, bits shifted out one end are
shifted into the opposite end. A value of **shift** greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of **shift** must be less than **size**. If the **size** argument is omitted,
it is taken to be equivalent to **bit_size(i)**.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **shift**
  : The type shall be _integer_.

- **size**
  : (Optional) The type shall be _integer_; the value must be greater than
  zero and less than or equal to **bit_size**(i).

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95 and later

### **See Also**

[**ishft**(3)](ISHFT)

_fortran-lang intrinsic descriptions_

## ishft

### **Name**

**ishft**(3) - \[BIT:SHIFT\] Shift bits

### **Syntax**

```fortran
result = ishft(i, shift)
```

### **Description**

**ishft**(3) returns a value corresponding to **i** with all of the bits shifted
**shift** places. A value of **shift** greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of **shift** is
greater than **bit_size(i)**, the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **shift**
  : The type shall be _integer_.

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95 and later

### **See Also**

[**ishftc**(3)](ISHFTC)

_fortran-lang intrinsic descriptions_

## is_iostat_end

### **Name**

**is_iostat_end**(3) - \[STATE\] Test for end-of-file value

### **Syntax**

```fortran
function is_iostat_end(i)

    logical function   :: is_iostat_end (i) result(yesno)
    integer,intent(in) :: i
```

### **Description**

is_iostat_end(3) tests whether a variable (assumed returned as a status
from an I/O statement) has the "end of file" I/O status value.

The function is equivalent to comparing the variable with the
**iostat_end** parameter of the intrinsic module **iso_fortran_env**.

### **Arguments**

- **i**
  : An _integer_ status value to test if indicating end of file.

### **Returns**

Returns a _logical_ of the default kind, **.true.** if **i** has the value
which indicates an end of file condition for **iostat=** specifiers, and is
**.false.** otherwise.

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

Fortran 2003 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## is_iostat_eor

### **Name**

**is_iostat_eor**(3) - \[STATE\] Test for end-of-record value

### **Syntax**

```fortran
result = is_iostat_eor(i)
```

### **Description**

is_iostat_eor tests whether an variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the iostat_eor parameter of the intrinsic module
**iso_fortran_env**.

### **Arguments**

- **i**
  : Shall be of the type _integer_.

### **Returns**

Returns a _logical_ of the default kind, which .true. if **i** has the value
which indicates an end of file condition for iostat= specifiers, and is
.false. otherwise.

### **Examples**

Sample program:

```fortran
program demo_is_iostat_eor
implicit none
integer :: stat, i(50)

  open(88, file='test.dat', form='unformatted')
  read(88, iostat=stat) i

  if(is_iostat_eor(stat)) stop 'end of record'

end program demo_is_iostat_eor
```

### **Standard**

Fortran 2003 and later

_fortran-lang intrinsic descriptions_

## kind

### **Name**

**kind**(3) - \[KIND INQUIRY\] Kind of an entity

### **Syntax**

```fortran
k = kind(x)
```

### **Description**

**kind(x)** returns the kind value of the entity **x**.

### **Arguments**

- **x**
  : Shall be of type _logical_, _integer_, _real_, _complex_ or _character_.

### **Returns**

The return value is a scalar of type _integer_ and of the default integer
kind.

### **Examples**

Sample program:

```fortran
program demo_kind
implicit none
integer,parameter :: kc = kind(' ')
integer,parameter :: kl = kind(.true.)

   print *, "The default character kind is ", kc
   print *, "The default logical kind is ", kl

end program demo_kind
```

Results:

```text
    The default character kind is            1
    The default logical kind is            4
```

### **Standard**

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## lbound

### **Name**

**lbound**(3) - \[ARRAY INQUIRY\] Lower dimension bounds of an array

### **Syntax**

```fortran
result = lbound(array, dim, kind)

   TYPE(kind=KIND) elemental function lbound(array,dim,kind)
   TYPE(kind=KIND),intent(in)  :: array
   integer,optional,intent(in) :: dim
   integer,optional,intent(in) :: kind
```

### **Description**

Returns the lower bounds of an array, or a single lower bound along the
**dim** dimension.

### **Arguments**

- **array**
  : Shall be an array, of any type.

- **dim**
  : Shall be a scalar _integer_.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

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

Fortran 95 and later, with KIND argument - Fortran 2003 and later

### **See Also**

[**ubound**(3)](UBOUND),
[**co_lbound**(3)](CO_LBOUND)

_fortran-lang intrinsic descriptions_

## leadz

### **Name**

**leadz**(3) - \[BIT:COUNT\] Number of leading zero bits of an integer

### **Syntax**

```fortran
result = leadz(i)
```

### **Description**

**leadz** returns the number of leading zero bits of an integer.

### **Arguments**

- **i**
  : Shall be of type _integer_.

### **Returns**

The type of the return value is the same as a default _integer_. If all
the bits of **i** are zero, the result value is **bit_size(i)**.

### **Examples**

Sample program:

```fortran
program demo_leadz
implicit none
integer :: value, i
character(len=80) :: f
  write(*,'(*(g0))')'BIT_SIZE=',bit_size(value)
  ! make a format statement for writing a value as a bit string
  write(f,'("(b",i0,".",i0,")")')bit_size(value),bit_size(value)
  ! show output for various integer values
  value=0
  do i=0,bit_size(value)-1
     write (*,'("LEADING ZERO BITS=",i3,1x)') leadz(value)
     write (*,'(" FOR VALUE ")',advance='no')
     write(*,f,advance='no') value
     write(*,'(*(1x,g0))') "OR",value
     value=value+2**(i)
  enddo
end program demo_leadz
```

Results:

```
   BIT_SIZE=32
   LEADING ZERO BITS= 32
    FOR VALUE 00000000000000000000000000000000 OR 0
   LEADING ZERO BITS= 31
    FOR VALUE 00000000000000000000000000000001 OR 1
   LEADING ZERO BITS= 30
    FOR VALUE 00000000000000000000000000000011 OR 3
   LEADING ZERO BITS= 29
    FOR VALUE 00000000000000000000000000000111 OR 7
   LEADING ZERO BITS= 28
    FOR VALUE 00000000000000000000000000001111 OR 15
   LEADING ZERO BITS= 27
    FOR VALUE 00000000000000000000000000011111 OR 31
   LEADING ZERO BITS= 26
    FOR VALUE 00000000000000000000000000111111 OR 63
   LEADING ZERO BITS= 25
    FOR VALUE 00000000000000000000000001111111 OR 127
   LEADING ZERO BITS= 24
    FOR VALUE 00000000000000000000000011111111 OR 255
   LEADING ZERO BITS= 23
    FOR VALUE 00000000000000000000000111111111 OR 511
   LEADING ZERO BITS= 22
    FOR VALUE 00000000000000000000001111111111 OR 1023
   LEADING ZERO BITS= 21
    FOR VALUE 00000000000000000000011111111111 OR 2047
   LEADING ZERO BITS= 20
    FOR VALUE 00000000000000000000111111111111 OR 4095
   LEADING ZERO BITS= 19
    FOR VALUE 00000000000000000001111111111111 OR 8191
   LEADING ZERO BITS= 18
    FOR VALUE 00000000000000000011111111111111 OR 16383
   LEADING ZERO BITS= 17
    FOR VALUE 00000000000000000111111111111111 OR 32767
   LEADING ZERO BITS= 16
    FOR VALUE 00000000000000001111111111111111 OR 65535
   LEADING ZERO BITS= 15
    FOR VALUE 00000000000000011111111111111111 OR 131071
   LEADING ZERO BITS= 14
    FOR VALUE 00000000000000111111111111111111 OR 262143
   LEADING ZERO BITS= 13
    FOR VALUE 00000000000001111111111111111111 OR 524287
   LEADING ZERO BITS= 12
    FOR VALUE 00000000000011111111111111111111 OR 1048575
   LEADING ZERO BITS= 11
    FOR VALUE 00000000000111111111111111111111 OR 2097151
   LEADING ZERO BITS= 10
    FOR VALUE 00000000001111111111111111111111 OR 4194303
   LEADING ZERO BITS=  9
    FOR VALUE 00000000011111111111111111111111 OR 8388607
   LEADING ZERO BITS=  8
    FOR VALUE 00000000111111111111111111111111 OR 16777215
   LEADING ZERO BITS=  7
    FOR VALUE 00000001111111111111111111111111 OR 33554431
   LEADING ZERO BITS=  6
    FOR VALUE 00000011111111111111111111111111 OR 67108863
   LEADING ZERO BITS=  5
    FOR VALUE 00000111111111111111111111111111 OR 134217727
   LEADING ZERO BITS=  4
    FOR VALUE 00001111111111111111111111111111 OR 268435455
   LEADING ZERO BITS=  3
    FOR VALUE 00011111111111111111111111111111 OR 536870911
   LEADING ZERO BITS=  2
    FOR VALUE 00111111111111111111111111111111 OR 1073741823
   LEADING ZERO BITS=  1
    FOR VALUE 01111111111111111111111111111111 OR 2147483647
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**bit_size**(3)](BIT_SIZE),
[**popcnt**(3)](POPCNT),
[**poppar**(3)](POPPAR),
[**trailz**(3)](TRAILZ)

_fortran-lang intrinsic descriptions_

## len

### **Name**

**len**(3) - \[CHARACTER\] Length of a character entity

### **Syntax**

```fortran
   l = len(string, kind)

    integer(kind=KIND) function len(string,kind) result(value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```

where the returned value is the same kind as the **KIND**, or of
the default kind if **KIND** is not specified.

### **Description**

**len(3)** Returns the length of a _character_ string.

If **string** is an array, the length of an element of **string**
is returned.

Note that **string** need not be defined when this intrinsic is invoked,
as only the length (not the content) of **string** is needed.

### **Arguments**

- **string**
  : Shall be a scalar or array of type _character_.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind.

### **Standard**

FORTRAN 77 and later; with **kind** argument - Fortran 2003 and later

### **Examples**

Sample program

```fortran
program demo_len
implicit none
character(len=40) :: string
character(len=:),allocatable :: astring
character(len=:),allocatable :: many_strings(:)
integer :: ii

   ii=len(string)
  write(*,*)'length =',ii

  ! the string length will be constant for the fixed-length variable
  string=' How long is this string? '
  write(*,'(a)')' ',string,repeat('=',len(string))

  ! the allocatable string length will be the length of LHS expression
  astring=' How long is this string? '
  write(*,'(a)')' ',astring,repeat('=',len(astring))

   ! you can also query the length (and other attributes) of a string
   ! using a "type parameter inquiry:" (available since fortran 2018)
   write(*,*)'length from type parameter inquiry=',string%len

   ! a scalar is returned for an array, as all values in a Fortran
   ! character array must be of the same length:

   ! define an allocatable array with a constructor ...
     many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
   write(*,*)
   write(*,*)'length of ALL elements of array=',len(many_strings)

   call proc_star(' how long? ')

contains

   subroutine proc_star(str)
   character(len=*),intent(in)  :: str
   character(len=:),allocatable :: str2
   ! the length of str can be used in the definitions of variables
   character(len=len(str))      :: str3

      if(allocated(str2))deallocate(str2)
      ! syntax for allocating a scalar string
      allocate(character(len=len(str)) :: str2)

      write(*,*)len(str),len(str2),len(str3)
      ! these are other allowable ways to define str2
      str2=str
      str2=repeat(' ',len(str))

   end subroutine proc_star

end program demo_len
```

Results:

```text

```

### **See Also**

len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines that
allow you to deal with leading and trailing blanks.

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),
  [**scan**(3)](SCAN),
  [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## len_trim

### **Name**

**len_trim**(3) - \[CHARACTER:WHITESPACE\] Length of a character entity without trailing blank characters

### **Syntax**

```fortran
   result = len_trim(string, kind)

    integer(kind=KIND) function len_trim(string,KIND) result (value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```

### **Description**

Returns the length of a character string, ignoring any trailing blanks.

### **Arguments**

- **string**
  : The input string whose length is to be measured.
  Shall be a scalar of type _character_

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default _integer_ kind.

### **Examples**

Sample program

```fortran
program demo_len_trim
implicit none
character(len=:),allocatable :: string
   string=' how long is this string?     '
   write(*,*)'LENGTH=',len(string)
   write(*,*)'TRIMMED LENGTH=',len_trim(string)
   !
   ELE:block ! elemental example
   character(len=:),allocatable :: tablet(:)
   tablet=[character(len=256) :: &
   & ' how long is this string?     ',&
   & 'and this one?']
      write(*,*)'LENGTH=            ',len(tablet)
      write(*,*)'TRIMMED LENGTH=    ',len_trim(tablet)
      write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
   endblock ELE
   !
end program demo_len_trim
```

Results:

```
    LENGTH=          30
    TRIMMED LENGTH=          25
    LENGTH=                     256
    TRIMMED LENGTH=              25          13
    SUM TRIMMED LENGTH=          38
```

### **Standard**

Fortran 95 and later, with **kind** argument - Fortran 2003
and later

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),
  [**scan**(3)](SCAN),
  [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**repeat**(3)](REPEAT),
  [**len**(3)](LEN),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## lge

### **Name**

**lge**(3) - \[CHARACTER:COMPARE\] Lexical greater than or equal

### **Syntax**

```fortran
result = lge(string_a, string_b)
```

### **Description**

Determines whether one string is lexically greater than or equal to
another string, where the two strings are interpreted as containing
ASCII character codes. If the String **a** and String **b** are not the same
length, the shorter is compared as if spaces were appended to it to form
a value that has the same length as the longer.

In general, the lexical comparison intrinsics **lge**(3), **lgt**(3), **lle**(3), and **llt**(3)
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

### **Arguments**

- **string_a**
  : Shall be of default _character_ type.

- **string_b**
  : Shall be of default _character_ type.

### **Returns**

Returns .true. if string_a \>= string_b, and .false. otherwise, based
on the ASCII ordering.

### **Standard**

FORTRAN 77 and later

### **See Also**

**\[\[lgt**(3), **\[\[lle**(3), **\[\[llt**(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),

[**scan**(3)](SCAN),
[**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## lgt

### **Name**

**lgt**(3) - \[CHARACTER:COMPARE\] Lexical greater than

### **Syntax**

```fortran
result = lgt(string_a, string_b)
```

### **Description**

Determines whether one string is lexically greater than another string,
where the two strings are interpreted as containing ASCII character
codes. If the String **a** and String **b** are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

### **Arguments**

- **string_a**
  : Shall be of default _character_ type.

- **string_b**
  : Shall be of default _character_ type.

### **Returns**

Returns .true. if string_a \> string_b, and .false. otherwise, based
on the ASCII ordering.

### **Standard**

FORTRAN 77 and later

### **See Also**

[**lge**(3)](LGE),
[**lle**(3)](LLE),
[**llt**(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),

[**scan**(3)](SCAN),
[**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## lle

### **Name**

**lle**(3) - \[CHARACTER:COMPARE\] Lexical less than or equal

### **Syntax**

```fortran
result = lle(str_a, str_b)

   character(len=*),intent(in) :: str_a, str_b

      or

   character(len=*),intent(in) :: str_a, str_b(*) logical :: result
```

### **Description**

Determines whether one string is lexically less than or equal to another
string, where the two strings are interpreted as containing ASCII
character codes. if the **string_a** and **string_b** are not the same length,
the shorter is compared as if spaces were appended to it to form a value
that has the same length as the longer. Leading spaces are significant.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

### **Arguments**

- **str_a**
  : variable or array of default _character_ type.

- **str_b**
  : variable or array of default _character_ type.

  if **str_a** and **str_b** are both arrays they must be of the
  same shape.

### **Returns**

- **result**
  Returns **.true.** if **STR_A \<= STR_B**, and **.false.** otherwise, based on
  the ASCII ordering.

### **Examples**

Sample program:

```fortran
program demo_lle
implicit none
integer             :: i
   write(*,'(*(a))')(char(i),i=32,126)
     write(*,*) lle('abc','ABC')              ! F lowercase is > uppercase
     write(*,*) lle('abc','abc  ')            ! T trailing spaces
     ! If both strings are of zero length the result is true.
     write(*,*) lle('','')                    ! T
     write(*,*) lle('','a')                   ! T the null string is padded
     write(*,*) lle('a','')                   ! F
     write(*,*) lle('abc',['abc','123'])      ! [T,F] scalar and array
     write(*,*) lle(['cba', '123'],'abc')     ! [F,T]
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

FORTRAN 77 and later

### **See Also**

[**lge**(3)](LGE),
[**lgt**(3),](LGT),
[**llt**(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),

[**scan**(3)](SCAN),
[**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## llt

### **Name**

**llt**(3) - \[CHARACTER:COMPARE\] Lexical less than

### **Syntax**

```fortran
result = llt(string_a, string_b)
```

### **Description**

Determines whether one string is lexically less than another string,
where the two strings are interpreted as containing ASCII character
codes. If the **string_a** and **string_b** are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

### **Arguments**

- **string_a**
  : Shall be of default _character_ type.

- **string_b**
  : Shall be of default _character_ type.

### **Returns**

Returns .true. if string_a \<= string_b, and .false. otherwise, based
on the ASCII ordering.

### **Standard**

FORTRAN 77 and later

### **See Also**

[**lge**(3)](LGE),
[**lgt**(3)](LGT),
[**lle**(3](LLE))

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL), [**adjustr**(3)](ADJUSTR), [**index**(3)](INDEX),
  [**scan**(3)](SCAN), [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT), [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## log10

### **Name**

**log10**(3) - \[MATHEMATICS\] Base 10 logarithm function

### **Syntax**

```fortran
result = log10(x)

   real(kind=KIND) elemental function log10(x)
   real(kind=KIND),intent(in) :: x
```

### **Description**

**log10(x)** computes the base 10 logarithm of **x**. This
is generally called the "common logarithm".

### **Arguments**

- **x**
  : A _real_ value > 0 to take the log of.

### **Returns**

The return value is of type _real_ . The kind type parameter is
the same as **x**.

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

FORTRAN 77 and later

_fortran-lang intrinsic descriptions_

## log_gamma

### **Name**

**log_gamma**(3) - \[MATHEMATICS\] Logarithm of the Gamma function

### **Syntax**

```fortran
x = log_gamma(x)
```

### **Description**

**log_gamma(x)** computes the natural logarithm of the absolute value of the Gamma function.

### **Arguments**

- **x**
  : Shall be of type _real_ and neither zero nor a negative integer.

### **Returns**

The return value is of type _real_ of the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_log_gamma
implicit none
real :: x = 1.0
   write(*,*)x,log_gamma(x) ! returns 0.0
end program demo_log_gamma
```

Results:

```text
      1.00000000       0.00000000
```

### **Standard**

Fortran 2008 and later

### **See Also**

Gamma function: [**gamma**(3)](GAMMA)

_fortran-lang intrinsic descriptions_

## logical

### **Name**

**logical**(3) - \[TYPE:LOGICAL\] Converts one kind of _logical_ variable to another

### **Syntax**

```fortran
result = logical(l, kind)

 logical(kind=KIND) function logical(L,KIND)
  logical(kind=INK),intent(in) :: L
  integer,intent(in),optional :: KIND
```

### **Description**

Converts one kind of _logical_ variable to another.

### **Arguments**

- **l**
  : The type shall be _logical_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is a _logical_ value equal to **l**, with a kind
corresponding to **kind**, or of the default logical kind if **kind** is not
given.

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

Fortran 95 and later, related ISO_FORTRAN_ENV module - fortran 2009

### **See Also**

[**int**(3)](INT),
[**real**(3)](REAL),
[**cmplx**(3)](CMPLX)

_fortran-lang intrinsic descriptions_

## log

### **Name**

**log**(3) - \[MATHEMATICS\] Logarithm function

### **Syntax**

```fortran
result = log(x)
```

### **Description**

**log(x)** computes the natural logarithm of **x**, i.e. the logarithm to
the base "e".

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

The return value is of type _real_ or _complex_. The kind type parameter is
the same as **x**. If **x** is _complex_, the imaginary part OMEGA is in the range

**-PI** \< OMEGA \<= PI.

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

FORTRAN 77 and later

_fortran-lang intrinsic descriptions_

## maskl

### **Name**

**maskl**(3) - \[BIT:SET\] Generates a left justified mask

### **Syntax**

```fortran
result = maskl(i, kind)

  integer elemental function maskl(i,kind)
  integer,intent(in),optional :: kind
```

### **Description**

**maskl(i\[, _kind_\])** has its leftmost **i** bits set to **1**, and the
remaining bits set to **0**.

### **Arguments**

- **i**
  : Shall be of type _integer_.
  Its value must be non-negative, and less than or equal to the
  number of bits for the kind of the result.

- **kind**
  : Shall be a scalar constant expression of type _integer_.

### **Returns**

The return value is of type _integer_. If **kind** is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

The leftmost **i** bits are set to 1 and the other bits are set to 0.

### **Examples**

Sample program:

```fortran
program demo_maskl
implicit none
integer :: i
   i=maskl(1)
   write(*,'(i0,1x,b0,/)') i,i
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskl([(i,i,i=1,bit_size(0))])
end program demo_maskl
```

Results:

```text
-2147483648 10000000000000000000000000000000

          0 0
-2147483648 10000000000000000000000000000000
-1073741824 11000000000000000000000000000000
 -536870912 11100000000000000000000000000000
 -268435456 11110000000000000000000000000000
 -134217728 11111000000000000000000000000000
  -67108864 11111100000000000000000000000000
  -33554432 11111110000000000000000000000000
  -16777216 11111111000000000000000000000000
   -8388608 11111111100000000000000000000000
   -4194304 11111111110000000000000000000000
   -2097152 11111111111000000000000000000000
   -1048576 11111111111100000000000000000000
    -524288 11111111111110000000000000000000
    -262144 11111111111111000000000000000000
    -131072 11111111111111100000000000000000
     -65536 11111111111111110000000000000000
     -32768 11111111111111111000000000000000
     -16384 11111111111111111100000000000000
      -8192 11111111111111111110000000000000
      -4096 11111111111111111111000000000000
      -2048 11111111111111111111100000000000
      -1024 11111111111111111111110000000000
       -512 11111111111111111111111000000000
       -256 11111111111111111111111100000000
       -128 11111111111111111111111110000000
        -64 11111111111111111111111111000000
        -32 11111111111111111111111111100000
        -16 11111111111111111111111111110000
         -8 11111111111111111111111111111000
         -4 11111111111111111111111111111100
         -2 11111111111111111111111111111110
         -1 11111111111111111111111111111111
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**maskr**(3)](MASKR)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## maskr

### **Name**

**maskr**(3) - \[BIT:SET\] Generates a right-justified mask

### **Syntax**

```fortran
result = maskr(i, kind)

  integer elemental function maskr(i,kind)
  integer,intent(in),optional :: kind
```

### **Description**

**maskr(i\[, kind\])** has its rightmost **i** bits set to 1, and the
remaining bits set to 0.

### **Arguments**

- **i**
  : Shall be of type _integer_.
  Its value must be non-negative, and less than or equal to the
  number of bits for the kind of the result.

- **kind**
  : Shall be a scalar constant expression of type _integer_.

### **Returns**

The return value is of type _integer_. If **kind** is present, it
specifies the kind value of the return type; otherwise, it is of the
default integer kind.

It has its rightmost **i** bits set to 1, and the remaining bits set to 0.

### **Example**

Sample program:

```fortrqn
program demo_maskr
implicit none
integer :: i
   i=maskr(1)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-1)
   i=maskr(5)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-5)
   i=maskr(11)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-11)
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskr([(i,i,i=0,bit_size(0))])
end program demo_maskr
```

Results:

```text
1 1 10000000000000000000000000000000

31 11111 111000000000000000000000000000

2047 11111111111 111000000000000000000000

          0 0
          1 1
          3 11
          7 111
         15 1111
         31 11111
         63 111111
        127 1111111
        255 11111111
        511 111111111
       1023 1111111111
       2047 11111111111
       4095 111111111111
       8191 1111111111111
      16383 11111111111111
      32767 111111111111111
      65535 1111111111111111
     131071 11111111111111111
     262143 111111111111111111
     524287 1111111111111111111
    1048575 11111111111111111111
    2097151 111111111111111111111
    4194303 1111111111111111111111
    8388607 11111111111111111111111
   16777215 111111111111111111111111
   33554431 1111111111111111111111111
   67108863 11111111111111111111111111
  134217727 111111111111111111111111111
  268435455 1111111111111111111111111111
  536870911 11111111111111111111111111111
 1073741823 111111111111111111111111111111
 2147483647 1111111111111111111111111111111
         -1 11111111111111111111111111111111
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**maskl**(3)](MASKL)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## matmul

### **Name**

**matmul**(3) - \[TRANSFORMATIONAL\] matrix multiplication

### **Syntax**

```fortran
result = matmul(matrix_a, matrix_b)
```

### **Description**

Performs a matrix multiplication on numeric or logical arguments.

### **Arguments**

- **matrix_a**
  : An array of _integer_, _real_, _complex_, or _logical_ type, with a rank of
  one or two.

- **matrix_b**
  : An array of _integer_, _real_, or _complex_ type if **matrix_a** is of a
  numeric type; otherwise, an array of _logical_ type. The rank shall be
  one or two, and the first (or only) dimension of **matrix_b** shall be
  equal to the last (or only) dimension of **matrix_a**.

### **Returns**

The matrix product of **matrix_a** and **matrix_b**. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

### **Standard**

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## maxexponent

### **Name**

**maxexponent**(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind

### **Syntax**

```fortran
result = maxexponent(x)
```

### **Description**

**maxexponent(x)** returns the maximum exponent in the model of the type
of **x**.

### **Arguments**

- **x**
  : Shall be of type _real_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## maxloc

### **Name**

**maxloc**(3) - \[ARRAY:LOCATION\] Location of the maximum value within an array

### **Syntax**

```fortran
result = maxloc(array, dim, mask) result = maxloc(array, mask)
```

### **Description**

Determines the location of the element in the array with the maximum
value, or, if the **dim** argument is supplied, determines the locations
of the maximum element along each row of the array in the **dim**
direction. If **mask** is present, only the elements for which **mask**
is **.true.** are considered. If more than one element in the array has
the maximum value, the location returned is that of the first such element
in array element order. If the array has zero size, or all of the elements
of **mask** are .false., then the result is an array of zeroes. Similarly,
if **dim** is supplied and all of the elements of **mask** along a given
row are zero, the result value for that row is zero.

### **Arguments**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : Shall be an array of type _logical_, and conformable with **array**.

### **Returns**

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
    ! when array bounds do not start with one remember MAXLOC(3) returns the
    ! offset relative to the lower bound-1 of the location of the maximum
    ! value, not the subscript of the maximum value. When the lower bound of
    ! the array is one, these values are the same. In other words, MAXLOC(3)
    ! returns the subscript of the value assuming the first subscript of the
    ! array is one no matter what the lower bound of the subscript actually
    ! is.
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

Fortran 95 and later

### **See Also**

[**max**(3)](MAX),
[**maxval**(3)](MAXVAL)

_fortran-lang intrinsic descriptions_

## max

### **Name**

**max**(3) - \[NUMERIC\] Maximum value of an argument list

### **Syntax**

```fortran
result = max(a1, a2, a3, ...)
```

### **Description**

Returns the argument with the largest (most positive) value.

### **Arguments**

- **a1**
  : The type shall be _integer_ or _real_.

- **a2,a3,...**
  : An expression of the same type and kind as **a1**.

### **Returns**

The return value corresponds to the maximum value among the arguments,
and has the same type and kind as the first argument.

The function is both elemental and allows for an arbitrary number of
arguments. This means if some elements are scalar and some are arrays
that all the arrays must be of the same size, and the returned value
will be an array that is the result as if multiple calls were made with
all scalar values with a single element of each array used in each call.
If called with all arrays the returned array is the same as if multiple
calls were made with **max(arr1(1),arr2(1), ...)** to
**max(arr1(N),arr2(N))**.

### **Examples**

Sample program

```fortran
program demo_max
implicit none
real :: arr1(4)= [10.0,11.0,30.0,-100.0]
real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]

  !! this is simple enough because it is not being called elementally
  !! because all arguments are scalar
  !!

  write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)

  !!
  !! this is all max(3) could do before it became an elemental
  !! function and is the most intuitive
  !! except that it can take an arbitrary number of options,
  !! which is not common in Fortran without
  !! declaring a lot of optional parameters.
  !!
  !! That is it unless you want to use the elemental features of max(3)!

  !! Error: Intrinsic    max    at (1) must have at least two arguments
  !!write(*,*)max(arr1)
  !! This does not work because it is like trying to return
  !! [(max(arr1(i)),i=1,size(arr1))]
  !! so it is trying to take the max of a single value.
  !! To find the largest element of an array
  !! call maxloc(3) or maxval(3).

  !! Error: Different shape for arguments 'a1' and 'a2' for intrinsic
  !! 'max' at (1) on dimension 1 (4 and 5)
  !!write(*,*)max(arr1,arr2)
  !! but this will return an array of
  !! [(max(arr1(N),arr2(N),N=1,size(arr1))]

  write(*,*)max(arr1,arr2(1:4))

  !! so this works only if all the arrays are the same size and
  !! you want an array of the largest Nth elements
  !! from the input arrays.
  !! maybe you wanted to do maxval([arr1,arr2]) or
  !! equivalently max(maxval(arr1),maxval(arr2))
  !! to find the single largest element in both arrays?

  !! compares all scalars to each member of array and
  !! returns array of size arr2

  write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2)

  !! Error: Different shape for arguments 'a5' and 'a6'
  !! for intrinsic 'max' at (1) on dimension 1 (5 and 4)
  !! write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2,arr1)
  !! as the same reason above when arrays are used
  !! (without scalar values) all the arrays must be the same size

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

FORTRAN 77 and later

### **See Also**

[**maxloc**(3)](MAXLOC),
[**maxval**(3)](MAXVAL),
[**min**(3)](MIN)

_fortran-lang intrinsic descriptions_

## maxval

### **Name**

**maxval**(3) - \[ARRAY REDUCTION\] determines the maximum value in an array or row

### **Syntax**

```fortran
result = maxval(array, dim, mask)
```

or

```fortran
result = maxval(array, mask)
```

### **Description**

Determines the maximum value of the elements in an array value, or, if
the **dim** argument is supplied, determines the maximum value along each
row of the array in the **dim** direction. If **mask** is present, only the
elements for which **mask** is **.true.** are considered. If the array has zero
size, or all of the elements of **mask** are .false., then the result is the
most negative number of the type and kind of **array** if **array** is numeric,
or a string of nulls if **array** is of character type.

### **Arguments**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : (Optional) Shall be an array of type _logical_, and conformable with
  **array**.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**max**(3)](MAX),
[**maxloc**(3)](MAXLOC)

_fortran-lang intrinsic descriptions_

## merge_bits

### **Name**

**merge_bits**(3) - \[BIT:COPY\] Merge bits using a mask

### **Syntax**

```fortran
result = merge_bits(i, j, mask)

    elemental function merge_bits(i,j,mask) result(r)
    integer(kind=KIND), intent(in) :: i, j, mask
    integer(kind=KIND) :: r
```

where the result and all input values have the same _integer_ type and
KIND with the exception that the mask and either **i** or **j** may be
a BOZ constant.

### **Description**

A common graphics operation in Ternary Raster Operations is to combine
bits from two different sources, generally referred to as bit-blending.
**merge_bits** performs a masked bit-blend of **i** and **j** using
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

### **Arguments**

- **i**
  : value to select bits from when the associated bit in the mask is **1**.

- **j**
  : value to select bits from when the associated bit in the mask is **0**.

- **mask**
  : a value whose bits are used as a mask to select bits from **i** and **j**

### **Returns**

The bits blended from **i** and **j** using the mask **mask**. It is the
same type as **i** if **i** is of type _integer_, otherwise the same type
as **j**.

### **Example**

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

Fortran 2008 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## merge

### **Name**

**merge**(3) - \[ARRAY CONSTRUCTION\] Merge variables

### **Syntax**

```fortran
result = merge(tsource, fsource, mask)
```

### **Description**

The elemental function **merge**(3) selects values from two arrays or
scalars according to a logical mask. The result is equal to an element
of **tsource** where the corresponding element of **mask** is **.true.**, or an
element of **fsource** when it is .false. .

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

### **Arguments**

- **tsource**
  : May be of any type, including user-defined.

- **fsource**
  : Shall be of the same type and type parameters as **tsource**.

- **mask**
  : Shall be of type _logical_.

Note that (currently) _character_ values must be of the same length.

### **Returns**

The result is of the same type and type parameters as **tsource**. For any
element the result is **tsource** if **mask** is true and **fsource** otherwise.

### **Examples**

The value of

```fortran
     merge (1.0, 0.0, k > 0)
```

is 1.0 for K=5 and 0.0 for K=**-2**.

```fortran
program demo_merge
implicit none
integer :: tvals(2,3), fvals(2,3), answer(2,3)
logical :: mask(2,3)
integer :: i
logical :: chooseleft

   tvals(1,:)=[  10, -60,  50 ]
   tvals(2,:)=[ -20,  40, -60 ]

   fvals(1,:)=[ 0, 3, 2 ]
   fvals(2,:)=[ 7, 4, 8 ]

   mask(1,:)=[ .true.,  .false., .true. ]
   mask(2,:)=[ .false., .false., .true. ]

   write(*,*)'mask of logicals'
   answer=merge( tvals, fvals, mask )
   call printme()

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

Fortran 95 and later

### **See Also**

[**pack**(3)](PACK),
[**spread**(3)](SPREAD),
[**unpack**(3)](UNPACK)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## minexponent

### **Name**

**minexponent**(3) - \[NUMERIC MODEL\] Minimum exponent of a real kind

### **Syntax**

```fortran
result = minexponent(x)
```

### **Description**

**minexponent(x)** returns the minimum exponent in the model of the type
of **x**.

### **Arguments**

- **x**
  : Shall be of type _real_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## minloc

### **Name**

**minloc**(3) - \[ARRAY:LOCATION\] Location of the minimum value within an array

### **Syntax**

```fortran
    result = minloc(array, dim, mask)
```

or

```fortran
    result = minloc(array, mask)
```

### **Description**

Determines the location of the element in the array with the minimum
value, or, if the **dim** argument is supplied, determines the locations of
the minimum element along each row of the array in the **dim** direction. If
**mask** is present, only the elements for which **mask** is **.true.** are
considered. If more than one element in the array has the minimum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of **mask** are
.false., then the result is an array of zeroes. Similarly, if **dim** is
supplied and all of the elements of **mask** along a given row are zero, the
result value for that row is zero.

### **Arguments**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of **array**, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : Shall be an array of type _logical_, and conformable with **array**.

### **Returns**

If **dim** is absent, the result is a rank-one array with a length equal to
the rank of **array**. If **dim** is present, the result is an array with a rank
one less than the rank of **array**, and a size corresponding to the size of
**array** with the **dim** dimension removed. If **dim** is present and **array** has a
rank of one, the result is a scalar. In all cases, the result is of
default _integer_ type.

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

Fortran 95 and later

### **See Also**

[**min**(3)](MIN),
[**minval**(3)](MINVAL)

_fortran-lang intrinsic descriptions_

## min

### **Name**

**min**(3) - \[NUMERIC\] Minimum value of an argument list

### **Syntax**

```fortran
result = min(a1, a2, a3, ... )
```

### **Description**

Returns the argument with the smallest (most negative) value.

### **Arguments**

- **a1**
  : The type shall be _integer_ or _real_.

- **a2, a3, ...**
  : An expression of the same type and kind as **a1**.

### **Returns**

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

FORTRAN 77 and later

### **See Also**

[**max**(3)](MAX),
[**minloc**(3)](MINLOC),
[**minval**(3)](MINVAL)

_fortran-lang intrinsic descriptions_

## minval

### **Name**

**minval**(3) - \[ARRAY REDUCTION\] Minimum value of an array

### **Syntax**

```fortran
result = minval(array, dim, mask) result = minval(array, mask)
```

### **Description**

Determines the minimum value of the elements in an array value, or, if
the **dim** argument is supplied, determines the minimum value along each
row of the array in the **dim** direction.

If **mask** is present, only the
elements for which **mask** is **.true.** are considered.

If the array has zero size, or all of the elements of **mask** are
.false., then the result is **huge(array)** if **array** is numeric, or a
string of **char(len=255)** characters if **array** is of character type.

### **Arguments**

- **array**
  : Shall be an array of type _integer_, _real_, or _character_.

- **dim**
  : (Optional) Shall be a scalar of type _integer_, with a value between
  one and the rank of ARRAY, inclusive. It may not be an optional
  dummy argument.

- **mask**
  : Shall be an array of type _logical_, and conformable with **array**.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**min**(3)](MIN),
[**minloc**(3)](MINLOC)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## mod

### **Name**

**mod**(3) - \[NUMERIC\] Remainder function

### **Syntax**

```fortran
result = mod(a, p)
```

### **Description**

**mod**(a,p) computes the remainder of the division of **a** by **p**.

### **Arguments**

- **a**
  : Shall be a scalar of type _integer_ or _real_.

- **p**
  : Shall be a scalar of the same type and kind as **a** and not equal to
  zero.

### **Returns**

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

FORTRAN 77 and later

### **See Also**

[**modulo**(3)](MODULO)

_fortran-lang intrinsic descriptions_

## modulo

### **Name**

**modulo**(3) - \[NUMERIC\] Modulo function

### **Syntax**

```fortran
result = modulo(a, p)
```

### **Description**

**modulo(a,p)** computes the **a** modulo **p**.

### **Arguments**

- **a**
  : Shall be a scalar of type _integer_ or _real_.

- **p**
  : Shall be a scalar of the same type and kind as **a**. It shall not be
  zero.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**mod**(3)](MOD)

_fortran-lang intrinsic descriptions_

## move_alloc

### **Name**

**move_alloc**(3) - \[\] Move allocation from one object to another

### **Syntax**

```fortran
call move_alloc(src, dest)
```

### **Description**

**move_alloc(src, dest)** moves the allocation from **src*( to
**dest*. **src** will become deallocated in the process.

### **Arguments**

- **src**
  : allocatable, **intent(inout)**, may be of any type and kind.

- **dest**
  : allocatable, **intent(out)**, shall be of the same type, kind and
  rank as **src*.

### **Examples**

Basic Sample program to allocate a bigger grid

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

Fortran 2003 and later

### **See Also**

[**allocated**(3)](ALLOCATED)

_fortran-lang intrinsic descriptions_

## mvbits

### **Name**

**mvbits**(3) - \[BIT:COPY\] reproduce bit patterns found in one integer in another

### **Syntax**

```fortran
call mvbits(from, frompos, len, to, topos)
```

### **Description**

**mvbits(3f)** copies a bit pattern found in a range of adjacent bits in
the _integer_ **from** to a specified position in another integer **to**
(which is of the same kind as **from**). It otherwise leaves the bits
in **to** as-is.

The bit positions copied must exist within the value of **from**.
That is, the values of **frompos+len-1** and **topos+len-1** must be
nonnegative and less than **bit_size**(from).

The bits are numbered **0** to **bit_size(i)-1**, from right to left.

### **Arguments**

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

  **to**
  : is set by copying the sequence of bits of length **len**,
  starting at position **frompos** of **from** to position **topos** of
  **to**. No other bits of **to** are altered. On return, the **len**
  bits of **to** starting at **topos** are equal to the value that
  the **len** bits of **from** starting at **frompos** had on entry.

- **topos**
  : A nonnegative _integer_ value indicating the starting location in
  **to** to place the specified copy of bits from **from**.
  **topos + len** must be less than or equal to **bit_size(to)**.

### **Example**

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

Fortran 95 and later

### **See Also**

[**btest**(3)](BTEST),
[**iand**(3)](IAND),
[**ibclr**(3)](IBCLR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),
[**ieor**(3)](IEOR),
[**ior**(3)](IOR),
[**not**(3)](NOT)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## nearest

### **Name**

**nearest**(3) - \[MODEL_COMPONENTS\] Nearest representable number

### **Syntax**

```fortran
result = nearest(x, s)
```

### **Description**

**nearest(x, s)** returns the processor-representable number nearest to
**x** in the direction indicated by the sign of **s**.

### **Arguments**

- **x**
  : Shall be of type _real_.

- **s**
  : Shall be of type _real_ and not equal to zero.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## new_line

### **Name**

**new_line**(3) - \[CHARACTER\] new-line character

### **Syntax**

```fortran
result = new_line(c)

   character(len=1,kind=kind(c)) :: new_line(c)
   character(len=1),intent(in) :: c(..)
```

### **Description**

**new_line(c)** returns the new-line character.

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

### **Arguments**

- **C**
  : The argument shall be a scalar or array of the type _character_.

### **Returns**

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

Fortran 2003 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## nint

### **Name**

**nint**(3) - \[TYPE:NUMERIC\] Nearest whole number

### **Syntax**

```fortran
    elemental function nint(x [, kind=NN]) result(ANSWER)
     real(kind=??),intent(in) :: x
     integer(kind=NN) :: ANSWER
```

### **Description**

**nint(x)** rounds its argument to the nearest whole number with its
sign preserved.

The user must ensure the value is a valid value for the range of the
**kind** returned. If the processor cannot represent the result in the kind
specified, the result is undefined.

If **x** is greater than zero, **nint(x)** has the value **int(x+0.5)**.

If **x** is less than or equal to zero, **nint(x)** has the value
**int(a-0.5)**.

### **Arguments**

- **x**
  : The type of the argument shall be _real_.

- **kind**
  : (Optional) A constant _integer_ expression indicating the kind
  parameter of the result. Otherwise, the kind type parameter is that
  of default _integer_ type.

### **Returns**

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
     1    5   -5
    -3   -3   -2   -2   -2
    -1   -1    0    1    1
     2    2    2    3    3
    Range limits for typical KINDS:
    1 127
    2 32767
    4 2147483647
    8 9223372036854775807
    Any KIND big enough? ICHECK=          16
    These are all wrong answers for    1.2345669499901444E+019
       0
         0
              0
    -9223372036854775808
```

### **Standard**

FORTRAN 77 and later, with KIND argument - Fortran 90 and later

### **See Also**

[**aint**(3)](AINT),
[**anint**(3)](ANINT),
[**int**(3)](INT),
[**selected_int_kind**(3)](SELECTED_INT_KIND),
[**ceiling**(3)](CEILING),
[**floor**(3)](FLOOR)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## norm2

### **Name**

**norm2**(3) - \[MATHEMATICS\] Euclidean vector norm

### **Syntax**

```fortran
result = norm2(array, dim)

real function result norm2(array, dim)

   real,intent(in) :: array(..)
   integer,intent(in),optional :: dim
```

### **Description**

Calculates the Euclidean vector norm (L_2 norm) of **array** along
dimension **dim**.

### **Arguments**

- **array**
  : Shall be an array of type _real_.

- **dim**
  : shall be a scalar of type _integer_ with a value in the
  range from **1** to **rank(array)**.

### **Returns**

The result is of the same type as **array**.

If **dim** is absent, a scalar with the square root of the sum of squares of
the elements of **array** is returned.

Otherwise, an array of rank **n-1**,
where **n** equals the rank of **array**, and a shape similar to that of **array**
with dimension DIM dropped is returned.

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

Fortran 2008 and later

### **See Also**

[**product**(3)](PRODUCT),
[**sum**(3)](SUM),
[**hypot**(3)](HYPOT)

_fortran-lang intrinsic descriptions_

## not

### **Name**

**not**(3) - \[BIT:LOGICAL\] Logical negation

### **Syntax**

```fortran
result = not(i)
```

### **Description**

NOT returns the bitwise Boolean inverse of I.

### **Arguments**

- **i**
  : The type shall be _integer_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**iand**(3)](IAND),
[**ior**(3)](IOR),
[**ieor**(3)](IEOR),
[**ibits**(3)](IBITS),
[**ibset**(3)](IBSET),

[**ibclr**(3)](IBCLR)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## null

### **Name**

**null**(3) - \[TRANSFORMATIONAL\] Function that returns a disassociated pointer

### **Syntax**

```fortran
ptr => null(mold)

```

### **Description**

Returns a disassociated pointer.

If **mold** is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In _Fortran 95_, **mold** is optional. Please note that _Fortran 2003_ includes cases where it is required.

### **Arguments**

- **mold**
  : (Optional) shall be a pointer of any association status and of any
  type.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**associated**(3)](ASSOCIATED)

_fortran-lang intrinsic descriptions_

## num_images

### **Name**

**num_images**(3) - \[COLLECTIVE\] Number of images

### **Syntax**

```fortran
result = num_images(distance, failed)
```

### **Description**

Returns the number of images.

### **Arguments**

- **distance**
  : (optional, **intent(in)**) Nonnegative scalar integer

- **failed**
  : (optional, **intent(in)**) Scalar logical expression

### **Returns**

Scalar default-kind _integer_. If **distance** is not present or has value 0,
the number of images in the current team is returned. For values smaller
or equal distance to the initial team, it returns the number of images
index on the ancestor team which has a distance of **distance** from the
invoking team. If **distance** is larger than the distance to the initial
team, the number of images of the initial team is returned. If **failed** is
not present the total number of images is returned; if it has the value
.true., the number of failed images is returned, otherwise, the number
of images which do have not the failed status.

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

Fortran 2008 and later. With DISTANCE or FAILED argument, TS 18508 or later

### **See Also**

[**this_image**(3)](THIS_IMAGE),
[**image_index**(3)](THIS_INDEX)

_fortran-lang intrinsic descriptions_

## out_of_range

### **Name**
**out_of_range**(3) - \[TYPE:NUMERIC\] Whether a value cannot be converted safely.

### **Syntax**
```fortran
  result = OUT_OF_RANGE (X, MOLD [, ROUND])

   logical,elemental               :: out_of_range
   type(TYPE,kind=KIND),intent(in) :: x
   type(TYPE,kind=KIND),intent(in) :: mold
   logical,intent(in),optional     :: round

   where TYPE may be _real_ or _integer_ of any available KIND.
```
### **Description**
   **out_of_range**(3) determines whether a value **x** can be converted
   safely to a _real_ or _integer_ variable the same type and kind as
   **mold**.

### **Arguments**
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

### **Returns**

From the standard:

   Case (i):     If MOLD is of type integer, and ROUND is absent or
                 present with the value false, the result is true
                 if and only if the value of X is an IEEE infinity or
                 NaN, or if the integer with largest magnitude that lies
                 between zero and X inclusive is not representable by
                 objects with the type and kind of MOLD.

   Case (ii):    If MOLD is of type integer, and ROUND is present with
                 the value true, the result is true if and only
                 if the value of X is an IEEE in   nity or NaN, or
                 if the integer nearest X, or the integer of greater
                 magnitude if two integers are equally near to X, is not
                 representable by objects with the type and kind of MOLD.

   Case (iii):   Otherwise, the result is true if and only if the value
                 of X is an IEEE in   nity or NaN that is not
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
   di   culties on many processors.

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

   FORTRAN 2018 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## pack

### **Name**

**pack**(3) - \[ARRAY CONSTRUCTION\] Pack an array into an array of rank one

### **Syntax**

```fortran
result = pack(array, mask,vector)

   TYPE(kind=KIND) function pack(array,mask,vector)
   TYPE(kind=KIND),option(in) :: array(*)
   logical  :: mask(*)
   TYPE(kind=KIND),option(in),optional :: vector(*)
```

where TYPE(kind=KIND) may be any type, where **array** and **vector**
and the returned value must by of the same type. **mask** may be a
scalar as well an an array.

### **Description**

Stores the elements of ARRAY in an array of rank one.

The beginning of the resulting array is made up of elements whose **mask**
equals **.true.**. Afterwards, positions are filled with elements taken from
**vector**.

### **Arguments**

- **array**
  : Shall be an array of any type.

- **mask**
  : Shall be an array of type _logical_ and of the same size as **array**.
  Alternatively, it may be a _logical_ scalar.

- **vector**
  : (Optional) shall be an array of the same type as **array** and of rank
  one. If present, the number of elements in **vector** shall be equal to
  or greater than the number of true elements in **mask**. If **mask** is
  scalar, the number of elements in **vector** shall be equal to or
  greater than the number of elements in **array**.

### **Returns**

The result is an array of rank one and the same type as that of **array**.
If **vector** is present, the result size is that of **vector**, the number of
**.true.** values in **mask** otherwise.

### **Examples**

Sample program:

```fortran
program demo_pack
implicit none
   call test1()
   call test2()
   call test3()
contains
!
subroutine test1()
! gathering nonzero elements from an array:
integer :: m(6)

   m = [ 1, 0, 0, 0, 5, 0 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)  ! "1 5"

end subroutine test1
!
subroutine test2()
! Gathering nonzero elements from an array and appending elements
! from VECTOR till the size of the mask array (or array size if the
! mask is scalar):
integer :: m(4)

   m = [ 1, 0, 0, 2 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])

end subroutine test2
!
subroutine test3()
! select strings whose second character is "a"
character(len=10) :: m(4)

m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
   write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )

end subroutine test3
!
end program demo_pack
```

Results:

```text
   1 5
   1 2 3 4
   bat        cat
```

### **Standard**

Fortran 95 and later

### **See Also**

[**merge**(3)](MERGE),
[**spread**(3)](SPREAD),
[**unpack**(3)](UNPACK)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## parity

### **Name**

**parity**(3) - \[TRANSFORMATIONAL\] Reduction with exclusive **OR**()

### **Syntax**

```fortran
result = parity(mask, dim)

    function parity(mask, dim)
    type(logical(kind=LKIND))                    :: dim
    type(logical(kind=LKIND)),intent(in)         :: mask(..)
    type(integer(kind=KIND)),intent(in),optional :: dim
```

where KIND and LKIND are any supported kind for the type.

### **Description**

Calculates the parity (i.e. the reduction using .xor.) of __mask__ along
dimension __dim__.

### **Arguments**

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __mask__.

### **Returns**

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
```

### **Standard**

Fortran 2008 and later

_fortran-lang intrinsic descriptions_

## popcnt

### **Name**

**popcnt**(3) - \[BIT:COUNT\] Number of bits set

### **Syntax**

```fortran
result = popcnt(i)
```

### **Description**

Returns the number of bits set in the binary representation of an
_integer_.

### **Arguments**

- **i**
  : Shall be of type _integer_.

### **Returns**

The return value is of type _integer_ and of the default integer kind.

### **Examples**

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
     print *, popcnt(127),       poppar(127)
     print *, popcnt(huge(0)), poppar(huge(0))
     print *, popcnt(huge(0_int8)), poppar(huge(0_int8))
     print *, popcnt(huge(0_int16)), poppar(huge(0_int16))
     print *, popcnt(huge(0_int32)), poppar(huge(0_int32))
     print *, popcnt(huge(0_int64)), poppar(huge(0_int64))
end program demo_popcnt
```

Results:

```text
        7           1
       31           1
        7           1
       15           1
       31           1
       63           1
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**poppar**(3)](POPPAR),
[**leadz**(3)](LEADZ),
[**trailz**(3)](TRAILZ)

_fortran-lang intrinsic descriptions_

## poppar

### **Name**

**poppar**(3) - \[BIT:COUNT\] Parity of the number of bits set

### **Syntax**

```fortran
result = poppar(i)
```

### **Description**

Returns the parity of an integer's binary representation (i.e., the
parity of the number of bits set).

### **Arguments**

- **i**
  : Shall be of type _integer_.

### **Returns**

The return value is equal to **0** if **i** has an even number of bits set and 1 if an odd
number of bits are set.

It is of type _integer_ and of the default _integer_ kind.

### **Examples**

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
   print  *,  popcnt(127),            poppar(127)
   print  *,  popcnt(huge(0_int8)),   poppar(huge(0_int8))
   print  *,  popcnt(huge(0_int16)),  poppar(huge(0_int16))
   print  *,  popcnt(huge(0_int32)),  poppar(huge(0_int32))
   print  *,  popcnt(huge(0_int64)),  poppar(huge(0_int64))
end program demo_popcnt
```

Results:

```text
              7           1
              7           1
             15           1
             31           1
             63           1
```

### **Standard**

Fortran 2008 and later

### **See Also**

[**popcnt**(3)](POPCNT),
[**leadz**(3)](LEADZ),
[**trailz**(3)](TRAILZ)

_fortran-lang intrinsic descriptions_

## precision

### **Name**

**precision**(3) - \[NUMERIC MODEL\] Decimal precision of a real kind

### **Syntax**

```fortran
result = precision(x)
```

### **Description**

**precision(x)** returns the decimal precision in the model of the type
of **x**.

### **Arguments**

- **x**
  : Shall be of type _real_ or _complex_.

### **Returns**

The return value is of type _integer_ and of the default integer kind.

### **Examples**

Sample program:

```fortran
program demo_precision
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x(2)
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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## present

### **Name**

**present**(3) - [STATE\] Determine whether an optional dummy argument
is specified

### **Syntax**

```fortran
result = present(a)

   function present (a)
   logical :: present
```

### **Description**

Determines whether an optional dummy argument is present.

### **Arguments**

- **a**
  : May be of any type and may be a pointer, scalar or array value,
  or a dummy procedure. It shall be the name of an optional dummy
  argument accessible within the current subroutine or function.

### **Returns**

Returns either **.true.** if the optional argument **a** is present,
or **.false.** otherwise.

### **Examples**

Sample program:

```fortran
program demo_present
implicit none
   write(*,*) func(), func(42)
contains

integer function func(x)
integer, intent(in), optional :: x
   if(present(x))then
     func=x**2
   else
     func=0
   endif
end function

end program demo_present
```

Results:

```text
     0        1764
```

### **Standard**

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## product

### **Name**

**product**(3) - \[ARRAY REDUCTION\] Product of array elements

### **Syntax**

```fortran
  result = product(array, dim, mask)

    NUMERIC,intent(in) :: array(..)
    integer,intent(in),optional :: dim
    logical,intent(in),optional :: mask(..)
```

where **NUMERIC** is any numeric type

### **Description**

Multiplies together all the selected elements of **array**, or along
dimension **dim** if the corresponding element in **mask** is **.true.**.

If **dim** is absent, a scalar with the product of all elements in **array** is
returned. (Note a zero-sized **array** returns **1**).

When **dim** is present, If the masked array has a dimension of one
(ie. is a vector) the result is a scalar. Otherwise, an array of rank
**n-1**, where **n** equals the rank of **array**, and a shape similar
to that of **array** with dimension **dim** dropped is returned.

### **Arguments**

- **array**
  : Shall be an array of type _integer_, _real_ or _complex_.

- **dim**
  : shall be a scalar of type _integer_ with a value in the
  range from **1 to n**, where **n** equals the rank of **array**.

- **mask**
  : shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Returns**

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
   print all, 'all elements have a false mask=>',product(array,mask=.false.)

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
   call print_matrix_int('negative values',product(box,mask=box < 0,dim=1))

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

Fortran 95 and later

### **See Also**

[**sum**(3)](SUM), note that an element by element multiplication is done
directly using the star character.

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## radix

### **Name**

**radix**(3) - \[NUMERIC MODEL\] Base of a model number

### **Syntax**

```fortran
result = radix(x)
```

### **Description**

**radix(x)** returns the base of the model representing the entity **x**.

### **Arguments**

- **x**
  : Shall be of type _integer_ or _real_

### **Returns**

The return value is a scalar of type _integer_ and of the default integer
kind.

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## random_number

### **Name**

**random_number**(3) - \[MATHEMATICS:RANDOM\] Pseudo-random number

### **Syntax**

```fortran
   random_number(harvest)
```

### **Description**

Returns a single pseudorandom number or an array of pseudorandom numbers
from the uniform distribution over the range 0 \<= x \< 1.

### **Arguments**

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

Fortran 95 and later

### **See Also**

[**random_seed**(3)](RANDOM_SEED)

_fortran-lang intrinsic descriptions_

## random_seed

### **Name**

**random_seed**(3) - \[MATHEMATICS:RANDOM\] Initialize a pseudo-random number sequence

### **Syntax**

```fortran
call random_seed(size, put, get)
```

### **Description**

Restarts or queries the state of the pseudorandom number generator used
by random_number.

If random_seed is called without arguments, it is seeded with random
data retrieved from the operating system.

### **Arguments**

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

Fortran 95 and later

### **See Also**

[**random_number**(3)](RANDOM_NUMBER)

_fortran-lang intrinsic descriptions_

## range

### **Name**

**range**(3) - \[NUMERIC MODEL\] Decimal exponent range of a real kind

### **Syntax**

```fortran
result = range(x)

      function range (x)
      integer :: range
      type(TYPE,kind=KIND),intent(in) :: x
```

where TYPE is _real_ or _complex_ and KIND is any kind supported by
TYPE.

### **Description**

**range(x)** returns the decimal exponent range in the model of the type
of **x**.

### **Arguments**

- **x**
  : Shall be of type _real_ or _complex_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## rank

### **Name**

**rank**(3) - \[ARRAY INQUIRY\] Rank of a data object

### **Syntax**

```fortran
result = rank(a)
```

### **Description**

**rank(a)** returns the rank of a scalar or array data object.

### **Arguments**

- **a**
  : can be of any type

### **Returns**

The return value is of type _integer_ and of the default integer kind. For
arrays, their rank is returned; for scalars zero is returned.

### **Examples**

Sample program:

```fortran
program demo_rank
implicit none
integer :: a
real, allocatable :: b(:,:)
real  :: c(10,20,30)
   print *, rank(a), rank(b), rank(c)
end program demo_rank
```

Results:

```text
   0           2           3
```

### **Standard**

TS 29113

_fortran-lang intrinsic descriptions_

## real

### **Name**

**real**(3) - \[TYPE:NUMERIC\] Convert to real type

### **Syntax**

```fortran
result = real(x, kind)
```

### **Description**

**real(x, kind)** converts its argument **x** to a real type.

For complex values this is similar to the modern complex-part-designator
**%RE** which also designates the real part of a value, accept a
designator can appear on the left-hand side of an assignment as well,
as in **val%re=(3.0,4.0)**.

### **Arguments**

- **x**
  : Shall be _integer_, _real_, or _complex_ to convert to _real_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

These functions return a _real_ variable or array under the following
rules:

1.  **real**(x) is converted to a default _real_ type if **x** is an _integer_
    or _real_ variable.

2.  **real**(x) is converted to a _real_ type with the magnitude of the _real_
    component of a complex value with kind type parameter of **x**.

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

FORTRAN 77 and later

### **See Also**

[**dble**(3)](DBLE),
[**cmplx**(3)](CMPLX),
[**aimag**(3)](AIMAG),
[**int**(3)](INT)

_fortran-lang intrinsic descriptions_

## reduce

### **Name**

**reduce**(3) - \[TRANSFORMATIONAL\] general reduction of an array

### **Syntax**

There are two forms to this function:
```fortran

   reduce(array, operation, mask, identity, ordered)
   reduce(array, operation, dim, mask, identity, ordered)
```

```fortran
      type(TYPE),intent(in)          :: array
      pure function                  :: operation
      integer,intent(in),optional    :: dim
      logical,optional               :: mask
      type(TYPE),intent(in),optional :: identity
      logical,intent(in),optional    :: ordered
```
   where TYPE may be of any type. TYPE must be the same for **array**
   and **identity**.

### **description**

   Reduce a list of conditionally selected values from an array to a
   single value by iteratively applying a binary function.

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

### **options**

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

### **result**

The result is of the same type and type parameters as **array**. It is
scalar if **dim** does not appear.

If **dim** is present, it indicates the one dimension along which to
perform the reduction, and the resultant array has a rank reduced by
one relative to the input array.

### **examples**

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
      !write(*,*)'positive value sum=',reduce(pack(arr,mask=arr>0), my_mult )
   ! sum values ignoring negative values
      write(*,*)'sum positive values=',reduce(arr, my_sum, mask=arr>0)
      !write(*,*)'sum positive values=',reduce(pack(arr,mask=arr>0), my_sum )

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

### **See Also**
- [co_reduce(3)](CO_REDUCE)
- [associative:wipipedia](https://en.wikipedia.org/wiki/Associative_property)

### **Standard**

   Fortran 2018

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## repeat

### **Name**

**repeat**(3) - \[CHARACTER\] Repeated string concatenation

### **Syntax**

```fortran
result = repeat(string, ncopies)

   character(len=len(string)*ncopies) :: repeat
   character(len=*),intent(in)        :: string
   integer,intent(in)                 :: ncopies
```

### **Description**

Concatenates **ncopies** copies of a string.

### **Arguments**

- **string**
  : The input string to repeatedly generate.
  Shall be scalar and of type _character_.

- **ncopies**
  : Number of copies to make of _string_, greater than or equal to zero (0).
  Shall be scalar and of type _integer_.

### **Returns**

A new scalar of type _character_ built up from **ncopies** copies of **string**.

### **Examples**

Sample program:

```fortran
program demo_repeat
implicit none
integer :: i
    write(*,'(a)') repeat("^v", 36)         ! line break
    write(*,'(a)') repeat("_", 72)          ! line break
    write(*,'(a)') repeat("1234567890", 7)  ! number line
    do i=80,0,-1 ! a simple progress bar
        write(*,'(a)',advance='no') &
        & repeat("#", i)//repeat(' ',80-i)//char(13)
        !do something slow
    enddo
end program demo_repeat
```

Results:

```
   ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
   ________________________________________________________________________
   1234567890123456789012345678901234567890123456789012345678901234567890
```

### **Standard**

Fortran 95 and later

### **See Also**

Functions that perform operations on character strings:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),
  [**scan**(3)](SCAN),
  [**verify**(3)](VERIFY)

- **Non-elemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## reshape

### **Name**

**reshape**(3) - \[ARRAY RESHAPE\] Function to reshape an array

### **Syntax**

```fortran
result = reshape(source, shape, pad, order)
```

### **Description**

Reshapes array **source** to correspond to **shape**. If necessary, the new
array may be padded with elements from **pad** or permuted as defined by
**order**.

### **Arguments**

- **source**
  : an array of any type.

- **shape**
  : an array of rank one and type _integer_. Its values must be positive
  or zero.

- **pad**
  : (Optional) an array of the same type as **source**.

- **order**
  : (Optional) an array of type _integer_ and the same shape as **shape**. Its
  values shall be a permutation of the numbers from 1 to n, where n is
  the size of **shape**. If **order** is absent, the natural ordering shall be
  assumed.

### **Returns**

The result is an array of shape **shape** with the same type as **source**.

### **Examples**

Sample program:

```fortran
program demo_reshape
implicit none
integer :: i
integer, dimension(4) :: x=[(i,i=10,40,10)]
real :: xx(3,4)
real,allocatable :: v(:)
    ! x is originally a vector with four elements
    write(*,*) shape(x) ! what is the current shape of the array?
    write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"

    ! pack any array into a vector
    xx=1.0
    v=reshape(xx,[size(xx)])
    write(*,*)shape(v),ubound(v)
end program demo_reshape
```

Results:

```text
              4
              2           2
             12          12
```

### **Standard**

Fortran 95 and later

### **See Also**

[**shape**(3)](SHAPE)

_fortran-lang intrinsic descriptions_

## rrspacing

### **Name**

**rrspacing**(3) - \[MODEL_COMPONENTS\] Reciprocal of the relative spacing

### **Syntax**

```fortran
result = rrspacing(x)
```

### **Description**

**rrspacing(x)** returns the reciprocal of the relative spacing of model
numbers near **x**.

### **Arguments**

- **x**
  : Shall be of type _real_.

### **Returns**

The return value is of the same type and kind as **x**. The value returned
is equal to **abs(fraction(x)) \* float(radix(x))\*\*digits(x)**.

### **Standard**

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## same_type_as

### **Name**

**same_type_as**(3) - \[STATE\] Query dynamic types for equality

### **Syntax**

```fortran
result = same_type_as(a, b)
```

### **Description**

Query dynamic types for equality.

### **Arguments**

- **a**
  : Shall be an object of extensible declared type or unlimited
  polymorphic.

- **b**
  : Shall be an object of extensible declared type or unlimited
  polymorphic.

### **Returns**

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of **a** is the same as the dynamic type of **b**.

### **Standard**

Fortran 2003 and later

### **See Also**

[**extends_type_of**(3)](EXTENDS_TYPE_OF)

_fortran-lang intrinsic descriptions_

## scale

### **Name**

**scale**(3) - \[MODEL_COMPONENTS\] Scale a real value by a whole power of the radix

### **Syntax**

```fortran
result = scale(x, i)

   real(kind=KIND),intent(in) :: x
   integer,intent(in)         :: i
```

### **Description**

**scale(x,i)** returns x \* **radix(x)\*\*i**.

### **Arguments**

- **x**
  : The type of the argument shall be a _real_.

- **i**
  : The type of the argument shall be a _integer_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## scan

### **Name**

**scan**(3) - \[CHARACTER:SEARCH\] Scan a string for the presence of a set of characters

### **Syntax**

```fortran
result = scan(string, set[, back [, kind]])
```

### **Description**

Scans a **string** for any of the characters in a **set** of characters.

If **back** is either absent or equals **.false.**, this function returns the
position of the leftmost character of **STRING** that is in **set**. If **back**
equals **.true.**, the rightmost position is returned. If no character of **set**
is found in **string**, the result is zero.

### **Arguments**

- **string**
  : Shall be of type _character_.

- **set**
  : Shall be of type _character_.

- **back**
  : (Optional) shall be of type _logical_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

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

Fortran 95 and later, with KIND argument - Fortran 2003 and later

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL), [**adjustr**(3)](ADJUSTR), [**index**(3)](INDEX),
  [**scan**(3)](SCAN), [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT), [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## selected_char_kind

### **Name**

**selected_char_kind**(3) - \[KIND\] Choose character kind such as "Unicode"

### **Syntax**

```fortran
result = selected_char_kind(name)
```

### **Description**

**selected_char_kind(name)** returns the kind value for the character
set named NAME, if a character set with such a name is supported, or
**-1** otherwise. Currently, supported character sets include "ASCII"
and "DEFAULT" (iwhich are equivalent), and "ISO_10646" (Universal
Character Set, UCS-4) which is commonly known as "Unicode".

### **Arguments**

- **name**
  : Shall be a scalar and of the default character type.

### **Examples**

Sample program:

```fortran
program demo_selected_char_kind
use iso_fortran_env
implicit none
integer, parameter :: ascii = selected_char_kind ("ascii")
integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

character(kind=ascii, len=26) :: alphabet
character(kind=ucs4,  len=30) :: hello_world

   alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
   hello_world = ucs4_'Hello World and Ni Hao -- ' &
                 // char (int (z'4F60'), ucs4)     &
                 // char (int (z'597D'), ucs4)

   write (*,*) alphabet

   open (output_unit, encoding='UTF-8')
   write (*,*) trim (hello_world)
end program demo_selected_char_kind
```

Results:

```text
    abcdefghijklmnopqrstuvwxyz
    Hello World and Ni Hao -- 你好
```

### **Standard**

Fortran 2003 and later

_fortran-lang intrinsic descriptions_

## selected_int_kind

### **Name**

**selected_int_kind**(3) - \[KIND\] Choose integer kind

### **Syntax**

```fortran
result = selected_int_kind(r)
```

### **Description**

**selected_int_kind(r)** return the kind value of the smallest integer
type that can represent all values ranging from **-10\*\*r** (exclusive)
to **10\*\*r** (exclusive). If there is no integer kind that accommodates
this range, selected_int_kind returns **-1**.

### **Arguments**

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

Fortran 95 and later

### **See Also**

[**aint**(3)](AINT),
[**anint**(3)](ANINT),
[**int**(3)](INT),
[**nint**(3)](NINT),
[**ceiling**(3)](CEILING),
[**floor**(3)](FLOOR)

_fortran-lang intrinsic descriptions_

## selected_real_kind

### **Name**

**selected_real_kind**(3) - \[KIND\] Choose real kind

### **Syntax**

```fortran
result = selected_real_kind(p, r, radix)
```

### **Description**

**selected_real_kind(p, r, radix)** return the kind value of a real
data type with decimal precision of at least **p** digits, exponent range of
at least **r**, and with a radix of **radix**.

### **Arguments**

- **p**
  : (Optional) shall be a scalar and of type _integer_.

- **r**
  : (Optional) shall be a scalar and of type _integer_.

- **radix**
  : (Optional) shall be a scalar and of type _integer_.

Before **Fortran 2008**, at least one of the arguments **r** or **p** shall
be present; since **Fortran 2008**, they are assumed to be zero if
absent.

### **Returns**

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

Fortran 95 and later; with RADIX - Fortran 2008 and later

### **See Also**

[**precision**(3)](PRECISION),
[**range**(3)](RANGE),
[**radix**(3)](RADIX)

_fortran-lang intrinsic descriptions_

## set_exponent

### **Name**

**set_exponent**(3) - \[MODEL_COMPONENTS\] Set the exponent of the model

### **Syntax**

```fortran
result = set_exponent(x, i)
```

### **Description**

**set_exponent(x, i)** returns the real number whose fractional part is
that of **x** and whose exponent part is **i**.

### **Arguments**

- **x**
  : Shall be of type _real_.

- **i**
  : Shall be of type _integer_.

### **Returns**

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

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**spacing**(3)](SPACING),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## shape

### **Name**

**shape**(3) - \[ARRAY INQUIRY\] Determine the shape of an array

### **Syntax**

```fortran
result = shape(source, kind)
```

### **Description**

Determines the shape of an array.

### **Arguments**

- **source**
  : Shall be an array or scalar of any type. If **source** is a pointer it
  must be associated and allocatable arrays must be allocated.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

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

Fortran 95 and later; with KIND argument Fortran 2003 and later

### **See Also**

[**reshape**(3)](RESHAPE),
[**size**(3)](SIZE)

_fortran-lang intrinsic descriptions_

## shifta

### **Name**

**shifta**(3) - \[BIT:SHIFT\] shift bits right with fill

### **Syntax**

```fortran
result = shifta(i, shift)
```

### **Description**

Returns a value corresponding to **i** with all of the bits shifted right by
**shift** places. If the absolute value of **shift** is greater than
**bit_size(i)**, the value is undefined. Bits shifted out from the
right end are lost. The fill is arithmetic: the bits shifted in from the
left end are equal to the leftmost bit, which in two's complement
representation is the sign bit.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **shift**
  : The type shall be _integer_.

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 2008 and later

### **See Also**

[**shiftl**(3)](SHIFTL),
[**shiftr**(3)](SHIFTR)

_fortran-lang intrinsic descriptions_

## shiftl

### **Name**

**shiftl**(3) - \[BIT:SHIFT\] shift bits left

### **Syntax**

```fortran
result = shiftl(i, shift)
```

### **Description**

Returns a value corresponding to **i** with all of the bits shifted left by
**shift** places. If the absolute value of **shift** is greater than
**bit_size(i)**, the value is undefined. Bits shifted out from the left
end are lost, and bits shifted in from the right end are set to **0**.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **shift**
  : The type shall be _integer_.

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 2008 and later

### **See Also**

[**shifta**(3)](SHIFTA),
[**shiftr**(3)](SHIFTR)

_fortran-lang intrinsic descriptions_

## shiftr

### **Name**

**shiftr**(3) - \[BIT:SHIFT\] shift bits right

### **Syntax**

```fortran
result = shiftr(i, shift)
```

### **Description**

Returns a value corresponding to **i** with all of the bits shifted right
by **shift** places. If the absolute value of **shift** is greater than
**bit_size(i)**, the value is undefined. Bits shifted out from the right
end are lost, and bits shifted in from the left end are set to 0.

### **Arguments**

- **i**
  : The type shall be _integer_.

- **shift**
  : The type shall be _integer_.

### **Returns**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 2008 and later

### **See Also**

[**shifta**(3)](SHIFTA),
[**shiftl**(3)](SHIFTL)

_fortran-lang intrinsic descriptions_

## sign

### **Name**

**sign**(3) - \[NUMERIC\] Sign copying function

### **Syntax**

```fortran
result = sign(a, b)

    elemental function sign(a, b)
    type(TYPE(kind=KIND))            :: sign
    type(TYPE(kind=KIND)),intent(in) :: a, b
```

where TYPE may be _real_ or _integer_ and KIND is any supported kind
for the type.

### **Description**

__sign__(a,b) return a value with the magnitude of __a__ but with the
sign of __b__.

For processors that distinguish between positive and negative zeros
__sign()__ may be used to distinguish between __real__ values 0.0 and
−0.0. SIGN (1.0, -0.0) will return −1.0 when a negative zero is
distinguishable.

### **Arguments**

  - **a**
    : The value whos magnitude will be returned. Shall be of type
    _integer_ or _real_

  - **b**
    : The value whose sign will be returned. Shall be of the same type
    and kind as **a**

### **Returns**

The kind of the return value is the magnitude of __a__ with the sign of
__b__. That is,

  - If __b \>= 0__ then the result is __abs(a)__
  - else if __b < 0__ it is -__abs(a)__.
  - if __b__ is _real_ and the processor distinguishes between __-0.0__
    and __0.0__ then the
    result is __-abs(a)__

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

FORTRAN 77 and later

_fortran-lang intrinsic descriptions (license: MIT)_

## sinh

### **Name**

**sinh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function

### **Syntax**

```fortran
result = sinh(x)

    elemental TYPE(kind=KIND) function sinh(x)
    TYPE(kind=KIND) :: x
```

Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

### **Description**

**sinh(x)** computes the hyperbolic sine of **x**.

The hyperbolic sine of x is defined mathematically as:

**sinh(x) = (exp(x) - exp(-x)) / 2.0**

If **x** is of type _complex_ its imaginary part is regarded as a value
in radians.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

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

Fortran 95 and later, for a complex argument Fortran 2008 or later

### **See Also**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[**asinh**(3)](ASINH)

_fortran-lang intrinsic descriptions_

## sin

### **Name**

**sin**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Sine function

### **Syntax**

```fortran
result = sin(x)

    elemental TYPE(kind=KIND) function sin(x)
    TYPE(kind=KIND) :: x
```

Where the returned value has the kind of the input value
and TYPE may be _real_ or _complex_

### **Description**

**sin(x)** computes the sine of an angle given the size of the angle in
radians.

The sine of an angle in a right-angled triangle is the ratio of the
length of the side opposite the given angle divided by the length of the
hypotenuse.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_ in radians.

### **Returns**

- **result**
  : The return value has the same type and kind as **x**.

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

FORTRAN 77 and later

### **See Also**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[**asin**(3)](ASIN),
[**cos**(3)](COS),
[**tan**(3)](TAN)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## size

### **Name**

**size**(3) - \[ARRAY INQUIRY\] Determine the size of an array

### **Syntax**

```fortran
result = size(array, dim, kind)
```

### **Description**

Determine the extent of **array** along a specified dimension **dim**,
or the total number of elements in **array** if **dim** is absent.

### **Arguments**

- **array**
  : be an array of any type. If **array** is a pointer it must be
  associated and allocatable arrays must be allocated.

- **dim**
  : shall be a scalar of type _integer_ and its value shall be
  in the range from 1 to n, where n equals the rank of **array**.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default _integer_ kind.

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

Fortran 95 and later, with **kind** argument - Fortran 2003 and later

### **See Also**

[**shape**(3)](SHAPE),
[__reshape__(3)])(RESHAPE)

_fortran-lang intrinsic descriptions_

## spacing

### **Name**

**spacing**(3) - \[MODEL_COMPONENTS\] Smallest distance between two numbers of a given type

### **Syntax**

```fortran
result = spacing(x)
```

### **Description**

Determines the distance between the argument **x** and the nearest adjacent
number of the same type.

### **Arguments**

- **x**
  : Shall be of type _real_.

### **Returns**

The result is of the same type as the input argument **x**.

### **Examples**

Sample program:

```fortran
program demo_spacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)

   write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
   write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
end program demo_spacing
```

Results:

```text
      1.19209290E-07
      2.2204460492503131E-016
```

### **Standard**

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**tiny**(3)](TINY)

_fortran-lang intrinsic descriptions_

## spread

### **Name**

**spread**(3) - \[ARRAY CONSTRUCTION\] Add a dimension to an array

### **Syntax**
```fortran
result = spread(source, dim, ncopies)

  TYPE(kind=KIND) function spread(source, dim, ncopies)

   TYPE(kind=KIND)    :: source(..)
   integer,intent(in) :: dim
   integer,intent(in) :: ncopies
```
### **Description**

Replicates a **source** array **ncopies** times along a specified
dimension **dim**.

If **source** is scalar, the shape of the result is (MAX (NCOPIES, 0)).
and each element of the result has a value equal to **source**.

### **Arguments**

- **source**
  : Shall be a scalar or an array of any type and a rank less than
  fifteen.

- **dim**
  : Shall be a scalar of type _integer_ with a value in the range from
  **1** to **n+1**, where **n** equals the rank of **source**.

- **ncopies**
  : Shall be a scalar of type _integer_.

### **Returns**

The result is an array of the same type as **source** and has rank **n+1**
where **n** equals the rank of **source**.

### **Examples**

Sample program:

```fortran
program demo_spread
implicit none
integer :: a = 1, b(2) = [ 1, 2 ]

   write(*,*) spread(a, 1, 2)            ! "1 1"
   write(*,*) spread(b, 1, 2)            ! "1 1 2 2"

end program demo_spread

program example_spread
!  Author:
!    John Burkardt, 03 July 2006
implicit none
     !
integer ( kind = 4 ) a1(4,3)
integer ( kind = 4 ) a2(3,4)
integer i
integer ( kind = 4 ) s
integer ( kind = 4 ) v(4)
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'TEST_SPREAD'
     write ( *, '(a)' ) '  SPREAD is a FORTRAN90 function which replicates'
     write ( *, '(a)' ) '  an array by adding a dimension.'
     write ( *, '(a)' ) ' '
     !
     s = 99
     !
     write ( *, '(a,i6)' ) '  Suppose we have a scalar S = ', s
     write ( *, '(a)' ) ' '
     !
     v = spread ( s, 1, 4 )
     !
     write ( *, '(a)' ) '  V = spread ( s, 1, 4 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 4'
     write ( *, '(a)' ) ' '
     write ( *, '(4i6)' ) v(1:4)
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  Now first reset V to (1,2,3,4)'
     v = [ 1, 2, 3, 4 ]
     !
     a1 = spread ( v, 2, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A1 = spread ( v, 2, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (2) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 4
       write ( *, '(3i6)' ) a1(i,1:3)
     end do
     !
     a2 = spread ( v, 1, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A2 = spread ( v, 1, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 3
       write ( *, '(4i6)' ) a2(i,1:4)
     end do
end program example_spread
```

### **Standard**

Fortran 95 and later

### **See Also**

[**merge**(3)](MERGE),
[**pack**(3)](PACK),
[**unpack**(3)](UNPACK)

_fortran-lang intrinsic descriptions_

## sqrt

### **Name**

**sqrt**(3) - \[MATHEMATICS\] Square-root function

### **Syntax**

```fortran
result = sqrt(x)

   TYPE(kind=KIND) elemental function sqrt(x) result(value)
   TYPE(kind=KIND),intent(in) :: x
   TYPE(kind=KIND) :: value
```

Where **TYPE** may be _real_ or _complex_ and **KIND** may be any
kind valid for the declared type.

### **Description**

**sqrt(x)** computes the principal square root of **x**.

In mathematics, a square root of a number **x** is a number **y** such
that **y\*y = x**.

The number whose square root is being considered is known as the
_radicand_.

Every nonnegative number _x_ has two square roots of the same unique
magnitude, one positive and one negative. The nonnegative square root
is called the principal square root.

The principal square root of 9 is 3, for example, even though (-3)\*(-3)
is also 9.

A _real_ radicand must be positive.

Square roots of negative numbers are a special case of complex numbers,
where the components of the _radicand_ need not be positive in order to
have a valid square root.

### **Arguments**

- **x**
  : If **x** is _real_ its value must be greater than or equal to zero.
  The type shall be _real_ or _complex_.

### **Returns**

The return value is of type _real_ or _complex_. The kind type parameter is
the same as **x**.

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

FORTRAN 77 and later

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## storage_size

### **Name**

**storage_size**(3) - \[BIT:INQUIRY\] Storage size in bits

### **Syntax**

```fortran
result = storage_size(a, kind)
```

### **Description**

Returns the storage size of argument **a** in bits.

### **Arguments**

- **a**
  : Shall be a scalar or array of any type.

- **kind**
  : (Optional) shall be a scalar integer constant expression.

### **Returns**

The result is a scalar integer with the kind type parameter specified by
**kind** (or default integer type if **kind** is missing). The result value is
the size expressed in bits for an element of an array that has the
dynamic type and type parameters of **a**.

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

Fortran 2008 and later

### **See Also**

[**c_sizeof**(3)](C_SIZEOF)

_fortran-lang intrinsic descriptions_

## sum

### **Name**

**sum**(3) - \[ARRAY REDUCTION\] sum the elements of an array

### **Syntax**

```fortran
   result = sum(array[, mask])
   result = sum(array, dim[, mask])
```

### **Description**

Adds the elements of ARRAY along dimension DIM if the corresponding
element in MASK is TRUE.

### **Arguments**

- **array**
  : Shall be an array of type _integer_, _real_ or _complex_.

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from 1 to n, where n equals the rank of ARRAY.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as ARRAY.

### **Returns**

The result is of the same type as ARRAY.

If **dim**(3) is absent, a scalar with the sum of all elements in ARRAY
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

Fortran 95 and later

### **See Also**

intrinsics

_fortran-lang intrinsic descriptions_

## system_clock

### **Name**

**system_clock**(3) - \[SYSTEM:TIME\] Return numeric data from a real-time clock.

### **Syntax**

```fortran
subroutine system_clock(count, count_rate, count_max)

   integer,intent(out),optional  :: count
   integer,intent(out),optional  :: count_rate
    ! or !
   real,intent(out),optional     :: count_rate
   integer,intent(out),optional  :: count_max
```

### **Description**

**system_clock** lets you measure durations of time with the precision of
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

### **Arguments**

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

Fortran 95 and later

### **See Also**

[**date_and_time**(3)](DATE_AND_TIME),
[**cpu_time**(3)](CPU_TIME)

_fortran-lang intrinsic descriptions_

## tanh

### **Name**

**tanh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic tangent function

### **Syntax**

```fortran
x = tanh(x)
```

### **Description**

**tanh(x)** computes the hyperbolic tangent of **x**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

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

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

### **See Also**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[**atanh**(3)](ATANH)

_fortran-lang intrinsic descriptions_

## tan

### **Name**

**tan**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Tangent function

### **Syntax**

```fortran
result = tan(x)
```

### **Description**

**tan(x)** computes the tangent of **x**.

### **Arguments**

- **x**
  : The type shall be _real_ or _complex_.

### **Returns**

The return value has the same type and kind as **x**.

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

FORTRAN 77 and later. For a complex argument, Fortran 2008 or later.

### **See Also**

[**atan**(3)](ATAN),
[**cos**(3)](COS),
[**sin**(3)](SIN)

_fortran-lang intrinsic descriptions_

## this_image

### **Name**

**this_image**(3) - \[COLLECTIVE\] Cosubscript index of this image

### **Syntax**

```fortran
result = this_image()
```
or
```
```fortran
result = this_image(distance)
```
or
```fortran
result = this_image(coarray, dim)
```

### **Description**

Returns the cosubscript for this image.

### **Arguments**

- **distance**
  : (optional, **intent(in)**) Nonnegative scalar integer (not permitted
  together with **coarray**).

- **coarray**
  : Coarray of any type (optional; if **dim** present, required).

- **dim**
  : default integer scalar (optional). If present, **dim** shall be between
  one and the corank of **coarray**.

### **Returns**

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

Fortran 2008 and later. With DISTANCE argument, TS 18508
or later

### **See Also**

[**num\_images**(3)](NUM_IMAGES),
[**image\_index**(3)](IMAGE_INDEX)

_fortran-lang intrinsic descriptions_
```

## tiny

### **Name**

**tiny**(3) - \[NUMERIC MODEL\] Smallest positive number of a real kind

### **Syntax**

```fortran
result = tiny(x)
   real(kind=KIND) function(x)
   real(kind=KIND) :: x
```

where KIND may be any kind supported by type _real_

### **Description**

**tiny(x)** returns the smallest positive (non zero) number of the type
and kind of **x**.

### **Arguments**

- **x**
  : Shall be of type _real_.

### **Returns**

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
 doubleprecision is from 2.2250738585072014E-308 to 1.7976931348623157E+308
```

### **Standard**

Fortran 95 and later

### **See Also**

[**digits**(3)](DIGITS),
[**epsilon**(3)](EPSILON),
[**exponent**(3)](EXPONENT),
[**fraction**(3)](FRACTION),
[**huge**(3)](HUGE),
[**maxexponent**(3)](MAXEXPONENT),
[**minexponent**(3)](MINEXPONENT),
[**nearest**(3)](NEAREST),
[**precision**(3)](PRECISION),
[**radix**(3)](RADIX),
[**range**(3)](RANGE),
[**rrspacing**(3)](RRSPACING),
[**scale**(3)](SCALE),
[**set_exponent**(3)](SET_EXPONENT),
[**spacing**(3)](SPACING)

_fortran-lang intrinsic descriptions_

## trailz

### **Name**

**trailz**(3) - \[BIT:COUNT\] Number of trailing zero bits of an integer

### **Syntax**

```fortran
   result = trailz(i) integer :: result
   integer(kind=NNN),intent(in) :: i
```

### **Description**

**trailz(3)** returns the number of trailing zero bits of an _integer_ value

### **Arguments**

- **i**
  : Shall be of type _integer_.

### **Returns**

The type of the return value is the default _integer_. If all the bits of
I are zero, the result value is **bit_size(i)**.

### **Examples**

Sample program:

```fortran
program demo_trailz
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
& int8, int16, int32, int64
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

Fortran 2008 and later

### **See Also**

[**bit_size**(3)](BIT_SIZE),
[**popcnt**(3)](POPCNT),
[**poppar**(3)](POPPAR),
[**leadz**(3)](LEADZ)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_

## transfer

### **Name**

**transfer**(3) - \[TYPE:MOLD\] Transfer bit patterns

### **Syntax**

```fortran
result = transfer(source, mold, size)
```

### **Description**

Interprets the bitwise representation of **source** in memory as if it
is the representation of a variable or array of the same type and type
parameters as **mold**.

This is approximately equivalent to the C concept of "casting" one
type to another.

### **Arguments**

- **source**
  : Shall be a scalar or an array of any type.

- **mold**
  : Shall be a scalar or an array of any type.

- **size**
  : (Optional) shall be a scalar of type _integer_.

### **Returns**

The result has the same type as **mold**, with the bit level representation
of **source**. If **size** is present, the result is a one-dimensional array of
length **size**. If **size** is absent but **mold** is an array (of any size or
shape), the result is a one-dimensional array of the minimum length
needed to contain the entirety of the bitwise representation of **source**.
If **size** is absent and **mold** is a scalar, the result is a scalar.

If the bitwise representation of the result is longer than that of
**source**, then the leading bits of the result correspond to those of
**source** and any trailing bits are filled arbitrarily.

When the resulting bit representation does not correspond to a valid
representation of a variable of the same type as **mold**, the results are
undefined, and subsequent operations on the result cannot be guaranteed
to produce sensible behavior. For example, it is possible to create
_logical_ variables for which **var** and .not. var both appear to be true.

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

Fortran 90 and later

_fortran-lang intrinsic descriptions_

## transpose

### **Name**

**transpose**(3) - \[ARRAY MANIPULATION\] Transpose an array of rank two

### **Syntax**

```fortran
result = transpose(matrix)
```

### **Description**

Transpose an array of rank two.

A array is transposed by interchanging the rows and columns of the given
matrix. That is, element (i, j) of the result has the value of element(j,
i) for all (i, j).

### **Arguments**

- **matrix**
  : The array to transpose, which shall be of any type and have a rank
  of two.

### **Returns**

The transpose of the input array.  The result has the same type as
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

Fortran 95 and later

_fortran-lang intrinsic descriptions_

## trim

### **Name**

**trim**(3) - \[CHARACTER:WHITESPACE\] Remove trailing blank characters of a string

### **Syntax**

```fortran
result = trim(string)
```

### **Description**

Removes trailing blank characters of a string.

### **Arguments**

- **string**
  : Shall be a scalar of type _character_.

### **Returns**

A scalar of type _character_ which length is that of **string** less the
number of trailing blanks.

### **Examples**

Sample program:

```fortran
program demo_trim
implicit none
character(len=10), parameter :: s = "gfortran  "
   write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks

   ! with/without trailing blanks
   write(*,*) len(s), len(trim('   leading'))
   write(*,*) len(s), len(trim('   trailing    '))
   write(*,*) len(s), len(trim('               '))

end program demo_trim
```

Results:

```text
      10           8
      10          10
      10          11
      10           0
```

### **Standard**

Fortran 95 and later

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),

[**scan**(3)](SCAN),
[**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions_

## ubound

### **Name**

**ubound**(3) - \[ARRAY INQUIRY\] Upper dimension bounds of an array

### **Syntax**

```fortran
result = ubound(array, dim, kind)
```

### **Description**

Returns the upper bounds of an array, or a single upper bound along the
**dim** dimension.

### **Arguments**

- **array**
  : Shall be an array, of any type.

- **dim**
  : (Optional) Shall be a scalar _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

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

Fortran 95 and later, with KIND argument Fortran 2003
and later

### **See Also**

[**lbound**(3)](LBOUND),
[**co_ubound**(3)](CO_UBOUND),
[__co\_lbound__(3)(CO_LBOUND)]

_fortran-lang intrinsic descriptions_

## unpack

### **Name**

**unpack**(3) - \[ARRAY CONSTRUCTION\] scatter the elements of a vector
into an array using a mask

### **Syntax**

```fortran
result = unpack(vector, mask, field)

 type(TYPE(kind=KIND)),intent(in) :: vector(:)
 logical,intent(in)               :: mask(..)
 type(TYPE(kind=KIND)),intent(in) :: field(..)
 type(TYPE(kind=KIND))            :: result(..)
```
### **Description**

Scatter the elements of **vector** into a copy of an array **field**
of any rank using _.true._ values from **mask** in array element order
to specify placement of the **vector** values.

So a copy of **field** is generated with select elements replaced with
values from **vector**. This allows for complex replacement patterns
that would be difficult when using array syntax or multiple assignment
statements, particularly when the replacements are conditional.

### **Arguments**

- **vector**
  : New values to place into specified locations in **field**. Shall
  be an array of any type and rank one. It shall have at least as many
  elements as **mask** has **.true.** values.

- **mask**
  : Shall be an array of type _logical_ that specifies which values
  in **field** are to be replaced with values from **vector**.

- **field**
  : The original data to be edited. Shall be of the same type and type
  parameters as **vector** and shall be conformable with **mask**.

### **Returns**

The result is an array of the same type and type parameters as **vector**
and the same shape as **mask**.

The element of the result that corresponds to the ith true element
of MASK, in array element order, has the value VECTOR (i) for i =
1, 2, . . ., t, where t is the number of true values in MASK. Each
other element has a value equal to FIELD if FIELD is scalar or to the
corresponding element of FIELD if it is an array.

The resulting array corresponds to **field** with **.true.** elements
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

Fortran 95 and later

### **See Also**

[**merge**(3)](MERGE),
[**pack**(3)](PACK),
[**spread**(3)](SPREAD)

_fortran-lang intrinsic descriptions_

## verify

### **Name**

**verify**(3) - \[CHARACTER:SEARCH\] Scan a string for the absence of a set of characters

### **Syntax**

```fortran
result = verify(string, set, back, kind)

  integer(kind=KIND) elemental function verify(string,set,back,kind)

   character(len=*),intent(in) :: string
   character(len=*),intent(in) :: set
   logical,intent(in),optional :: back
   integer,intent(in),optional :: KIND
```

### **Description**

Verifies that all the characters in **string** belong to the set of
characters in **set** by identifying the first character in the string(s)
that is not in the set(s).

If **back** is either absent or equals **.false.**, this function
returns the position of the leftmost character of **string** that is
not in **set**.

If **back** equals **.true.**, the rightmost position is returned.

If all characters of **string** are found in **set**, the result is zero.

This makes it easy to verify strings are all uppercase or lowercase,
follow a basic syntax, only contain printable characters, and many of the
conditions tested for with the C routines
**isalnum**(3c), **isalpha**(3c), **isascii**(3c), **isblank**(3c),
**iscntrl**(3c), **isdigit**(3c), **isgraph**(3c), **islower**(3c),
**isprint**(3c), **ispunct**(3c), **isspace**(3c), **isupper**(3c),
and **isxdigit**(3c); but for a string as well an an array of characters.

### **Arguments**

- **string**
  : Shall be of type _character_.

- **set**
  : Shall be of type _character_.

- **back**
  : shall be of type _logical_.

- **kind**
  : An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Returns**

The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default integer kind.

### **Examples**

Sample program I:

```fortran
program demo_verify
implicit none
character(len=*),parameter :: int='0123456789'
character(len=*),parameter :: hex='abcdef0123456789'
character(len=*),parameter :: low='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter :: upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=20):: string='   Howdy There!'
character(len=6) :: strings(2)=["Howdy ","there!"]
character(len=2) :: sets(2)=["de","gh"]

   write(*,*)'first non-blank character ',verify(string, ' ')
   ! NOTE: same as len_trim(3)
   write(*,*)'last non-blank character',verify(string, ' ',back=.true.)

   ! first non-lowercase non-blank character
   write(*,*) verify(string,low//' ')

   !! elemental -- using arrays for both strings and for sets

   ! first character in "Howdy" not in "de", and first letter in "there!"
   ! not in "gh"
   write(*,*) verify(strings,sets)

   ! check each string from right to left for non-letter
   write(*,*) 'last non-letter',verify(strings,upp//low,back=.true.)

   ! note character variables in an array have to be of same length
   ! find last non-uppercase character in "Howdy"
   ! and first non-lowercase in "There!"
   write(*,*) verify(strings,[upp,low],back=[.true.,.false.])

   write(*,*) verify("fortran", "", .true.)  ! 7, found 'n'
   ! 0' found none unmatched
   write(*,*) verify("fortran", "nartrof")

    !! CHECK IF STRING IS OF FORM NN-HHHHH
    CHECK : block
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
       endif

    endblock CHECK
end program demo_verify
```

Results:

```text
    first non-blank character            4
    last non-blank character          15
              4
              1           1
    last non-letter           6           6
              6           6
              7
              0
    32-af43d passed
```

Sample program II:

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

   write(*,'("|",*(g0,"|"))') ints
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
   if( name .eq. '' )return
   ! allow one leading sign
   if( verify(name(1:1),'+-') == 0 ) name=name(2:)
   ! was just a sign
   if( name .eq. '' )return
   lout=verify(trim(name), digits)  == 0
end function isint

end program fortran_ints
```

Results:

```text
|+1       |3044848  |30.40    |September|1 2 3    |  -3000  |         |
| T       | T       | F       | F       | F       | T       | F       |
```

Sample program III:

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

### **Standard**

Fortran 95 and later, with **kind** argument - Fortran 2003 and later

### **See Also**

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- **Elemental:**
  [**adjustl**(3)](ADJUSTL),
  [**adjustr**(3)](ADJUSTR),
  [**index**(3)](INDEX),
  [**scan**(3)](SCAN),
  [**verify**(3)](VERIFY)

- **Nonelemental:**
  [**len_trim**(3)](LEN_TRIM),
  [**len**(3)](LEN),
  [**repeat**(3)](REPEAT),
  [**trim**(3)](TRIM)

_fortran-lang intrinsic descriptions (license: MIT) @urbanjost_
