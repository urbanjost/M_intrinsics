## log

### **Name**

**log**(3) - \[MATHEMATICS\] Natural logarithm

### **Synopsis**
```fortran
  result = log(x)
```
```fortran
   elemental TYPE(kind=KIND) function log(x)

    TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **x** may be any _real_ or _complex_ kind.
 - the result is the same type and characteristics as **x**.

### **Description**

  **log**(3) computes the natural logarithm of **x**, i.e. the logarithm to
  the base "e".

### **Options**

- **x**
  : The value to compute the natural log of.
    If **x** is _real_, its value shall be greater than zero.
    If **x** is _complex_, its value shall not be zero.


### **Result**

  The natural logarithm of **x**.
  If **x** is the _complex_ value **(r,i)** , the imaginary part "i" is in the range
```fortran
    -PI < i <= PI
```
   If the real part of **x** is less than zero and the imaginary part of
   **x** is zero, then the imaginary part of the result is approximately
   **PI** if the imaginary part of **PI** is positive real zero or the
   processor does not distinguish between positive and negative real zero,
   and approximately **-PI** if the imaginary part of **x** is negative
   real zero.

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
  >    2.7182818284590451        1.0000000000000000
  > (1.00000000,2.00000000) (0.804718971,1.10714877)
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

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
