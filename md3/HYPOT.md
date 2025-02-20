## hypot

### **Name**

**hypot**(3) - \[MATHEMATICS\] Returns the Euclidean distance - the distance between a point and the origin.

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

 - **x,y** and the result shall all be _real_ and of the same **kind**.

### **Description**

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.

**hypot(x,y)** returns the special case of the Euclidean distance between
the point **<x,y>** and the origin. It is equal to
```fortran
sqrt(x**2+y**2)
```
without undue underflow or overflow.

### **Options**

- **x**
: the x value of the point of interest

- **y**
: the y value of the point of interest

### **Result**

The result is the positive magnitude of the distance of the point
**<x,y>** from the origin **<0.0,0.0>** .

### **Examples**

Sample program:

```fortran
program demo_hypot
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
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
 >
 > point <1.00000000,0.500000000> is 1.11803401
 > units away from the origin
 >
 > the points
 >    +1.00000000 +0.500000000
 >    +1.00000000 +0.250000000
 >    +10.0000000 -10.0000000
 >    +15.0000000 +0.250000000
 >    -1.00000000 -0.250000000
 > have distances from the origin of
 >    +1.11803401 +1.03077638
 >    +14.1421356 +15.0020828
 >    +1.03077638
 > the closest is
 >    +1.03077638
```
### **Standard**

Fortran 2008

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
