## log10

### **Name**

**log10**(3) - \[MATHEMATICS\] Base 10 or common logarithm

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
 > log10(1.000000000000000) is .000000000000000
 >   0.0000000E+00   1.000000       2.000000       3.000000       4.000000
 >    5.000000       6.000000       7.000000
```
### **Standard**

FORTRAN 77

### **See also**

 - [acos(3)](#acos)   - Arccosine (inverse cosine) function
 - [acosh(3)](#acosh) - Inverse hyperbolic cosine function
 - [asin(3)](#asin)   - Arcsine function
 - [asinh(3)](#asinh) - Inverse hyperbolic sine function
 - [atan(3)](#atan)   - Arctangent AKA inverse tangent function
 - [atan2(3)](#atan2) - Arctangent (inverse tangent) function
 - [atanh(3)](#atanh) - Inverse hyperbolic tangent function
 - [cos(3)](#cos)     - Cosine function
 - [cosh(3)](#cosh)   - Hyperbolic cosine function
 - [sin(3)](#sin)     - Sine function
 - [sinh(3)](#sinh)   - Hyperbolic sine function
 - [tan(3)](#tan)     - Tangent function
 - [tanh(3)](#tanh)   - Hyperbolic tangent function
 - [bessel_j0(3)](#bessel_j0) -  Bessel function of the first kind of order 0
 - [bessel_j1(3)](#bessel_j1) -  Bessel function of the first kind of order 1
 - [bessel_jn(3)](#bessel_jn) -  Bessel function of the first kind
 - [bessel_y0(3)](#bessel_y0) -  Bessel function of the second kind of order 0
 - [bessel_y1(3)](#bessel_y1) -  Bessel function of the second kind of order 1
 - [bessel_yn(3)](#bessel_y2) -  Bessel function of the second kind
 - [erf(3)](#erf)     -  Error function
 - [erfc(3)](#erfc)   -  Complementary error function
 - [erfc_scaled(3)](#erfc_scaled) -  Scaled complementary error function
 - [exp(3)](#exp)     -  Base-e exponential function
 - [gamma(3)](#gamma) -  Gamma function, which yields factorials for positive whole numbers
 - [hypot(3)](#hypot) -  Returns the Euclidean distance - the distance between a point and the origin.
 - [log(3)](#log)     -  Natural logarithm
 - [log10(3)](#log10) -  Base 10 or common logarithm
 - [log_gamma(3)](#log_gamma) -  Logarithm of the absolute value of the Gamma function
 - [norm2(3)](#norm2) -  Euclidean vector norm
 - [sqrt(3)](#sqrt)   -  Square-root function
 - [random_init(3)](#random_init) - Initializes the state of the pseudorandom number generator
 - [random_number(3)](#random_number) - Pseudo-random number
 - [random_seed(3)](#random_seed) - Initialize a pseudo-random number sequence

 _Fortran intrinsic descriptions_
