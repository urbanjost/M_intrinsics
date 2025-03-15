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

! some values to use with ABS(3)
   integer           :: i = -1
   real              :: x = -1.0
   complex           :: z = (-3.0,-4.0)
   doubleprecision   :: rr = -45.78_dp

! some formats for pretty-printing some information
   character(len=*),parameter :: &
      frmt  =  '(1x,a15,1x," In: ",g0,            T51," Out: ",g0)', &
      frmtc = '(1x,a15,1x," In: (",g0,",",g0,")",T51," Out: ",g0)',  &
      gen   = '(*(g0,1x))'

   ! the basics
   print gen,  'basic usage:'
   ! any integer, real, or complex type
   write(*, frmt)  'integer         ',  i, abs(i)
   write(*, frmt)  'real            ',  x, abs(x)
   write(*, frmt)  'doubleprecision ', rr, abs(rr)
   write(*, frmtc) 'complex         ',  z, abs(z)

   ! elemental
   print gen, 'abs is elemental:', abs([20,  0,  -1,  -3,  100])

   ! the returned value for complex input can be thought of as the
   ! distance from the origin <0,0>
   print gen, 'distance of (', z, ') from zero is', abs( z )

   call DUSTY_CORNERS_1("beware of abs(-huge(0)-1)")
   call DUSTY_CORNERS_2("beware of losing precision using CMPLX(3)")
   call DUSTY_CORNERS_3("beware of overflow of complex values")
   call DUSTY_CORNERS_4("custom meaning for absolute value of COMPLEX")

contains

   subroutine DUSTY_CORNERS_1(message)
   character(len=*),intent(in) :: message

     ! A dusty corner is that abs(-huge(0)-1) of an integer would be
     ! a representable negative value on most machines but result in a
     ! positive value out of range.

     print gen,  message
     ! By definition:
     !   You can take the absolute value of any value whose POSITIVE value
     !   is representable with the same type and kind.

     print gen, 'abs range test : ', abs(huge(0)), abs(-huge(0))
     print gen, 'abs range test : ', abs(huge(0.0)), abs(-huge(0.0))
     print gen, 'abs range test : ', abs(tiny(0.0)), abs(-tiny(0.0))

   end subroutine DUSTY_CORNERS_1

   subroutine DUSTY_CORNERS_2(message)
   character(len=*),intent(in) :: message

     ! dusty corner: "kind=dp" is required or the value returned by
     ! CMPLX() is a default real instead of double precision.

     ! Working with complex values you often encounter the CMPLX(3)
     ! function. CMPLX(3) defaults to returning a default REAL regardless
     ! of input type. Not really a direct problem with ABS(2f) per-se,
     ! but a common error when working with doubleprecision complex values

     print gen,  message
     print gen, 'real result versus doubleprecision result', &
     & abs(cmplx(30.0_dp,40.0_dp)), &
     & abs(cmplx(30.0_dp,40.0_dp,kind=dp))

   end subroutine DUSTY_CORNERS_2

   subroutine DUSTY_CORNERS_3(message)
   character(len=*),intent(in) :: message
     print gen, message

     ! this will probably cause an overflow error, or
     !print gen,  abs(cmplx( huge(0.0), huge(0.0) ))

     print gen, 'because the biggest default real is',huge(0.0)
     print gen, 'because returning magnitude of sqrt(x%re**2,x%im**2)'

   end subroutine DUSTY_CORNERS_3

   subroutine DUSTY_CORNERS_4(message)
   character(len=*),intent(in) :: message
     print gen, message

     ! if you do not want the distance for a complex value you
     ! might want something like returning a complex value with
     ! both the imaginary and real parts. One way to do that is

     print gen, cmplx(abs(z%re),abs(z%im),kind=kind(z))

   end subroutine DUSTY_CORNERS_4

end program demo_abs
```
Results:
```text
 >  integer          In: -1                        Out: 1
 >  real             In: -1.00000000               Out: 1.00000000
 >  doubleprecision  In: -45.78000000000000        Out: 45.78000000000000
 >  complex          In: (-3.00000000,-4.00000000) Out: 5.00000000
 > abs is elemental: 20 0 1 3 100
 > distance of ( -3.00000000 -4.00000000 ) from zero is 5.00000000
 > beware of abs(-huge(0)-1)
 > abs range test :  2147483647 2147483647
 > abs range test :  0.340282347E+39 0.340282347E+39
 > abs range test :  0.117549435E-37 0.117549435E-37
 > beware of losing precision using CMPLX(3)
 > real result versus doubleprecision result 50.0000000 50.00000000000000
 > beware of overflow of complex values
 > because the biggest default real is 0.340282347E+39
 > because returning magnitude of sqrt(x%re**2,x%im**2)
 > making your own meaning for ABS(COMPLEX_VALUE)
 > 3.00000000 4.00000000
```
### **Standard**

   FORTRAN 77

### **See Also**

[**sign**(3)](#sign)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
