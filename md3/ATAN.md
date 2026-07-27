## atan

### **Name**

**atan**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent AKA inverse tangent function

### **Synopsis**
```fortran
    result = atan(x) | atan(y, x)
```
```fortran
     elemental TYPE(kind=KIND) function atan(y,x)

      TYPE(kind=KIND),intent(in) :: x
      TYPE(kind=KIND),intent(in),optional :: y
```
### **Characteristics**

 - If **y** is present **x** and **y** must both be _real_.
   Otherwise, **x** may be _complex_.
 - **KIND** can be any kind supported by the associated type.
 - The returned value is of the same type and kind as **x**.

### **Description**

**atan(x)**(3) returns the inverse tangent (ie. arctangent) of the
elements of **x** in radians. The function accepts both real and complex
inputs, and is elemental (therefore allowing arguments to be scalar,
vector, or matrix). The atan operation is performed element-wise when
X is nonscalar.

  * For real values of X, atan(X) returns values in the interval
    [-PI/2, PI/2].
  * For complex values of X, atan(X) returns complex values.

    When x is complex, Fortran’s intrinsic ATAN(x) computes the
    principal value of the complex arctangent function and returns a
    complex number in radians. The Imaginary part is an  unbounded real
    value representing the hyperbolic growth of the inverse function.

     - Converts complex coordinates using the natural logarithm and
       imaginary unit.
     - Reduces to the standard real arctangent when the input has a zero
       imaginary component.
     - Undefined at the exact poles.
     - Branch cuts lie along the outer imaginary axis

When Y is not supplied the inverse tangent is defined as

    i=sqrt(-1)
    atan(z)=>(i/2)*log(i+z/i-z).

This definition of the atan function returns angles in radians within
the interval [-PI/2, PI/2]. To find the four-quadrant inverse tangent,
where the returned angles are in the interval [-PI, PI], supply the
**y** value or equivalently, use atan2(3).

### **Options**

- **x**
  : The value to compute the arctangent of.
  if **y** is present, **x** shall be _real_.

- **y**
  : is of the same type and kind as **x**. If **x** is zero, **y**
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
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
character(len=*),parameter  :: g='(*(g0,1x))'
real(kind=real64),parameter :: &
 Deg_Per_Rad = 57.2957795130823208767981548_real64
real(kind=real64)           :: x
real(kind=real64),parameter              :: &

 xvals(*)=[2.0d0, 2.0d0, 2.0d0,  2.0d0,  -2.0d0, -2.0d0, -2.0d0, -2.0d0 ]
real(kind=real64),parameter              :: &
 yvals(*)=[2.0d0, 2.0d0, -2.0d0, -2.0d0, 2.0d0,  2.0d0,  -2.0d0, -2.0d0 ]
   !
   ! basics
   !
   ! with just a real X returns angles in radians 
   ! in the interval [-PI/2, PI/2].
    x=2.866_real64
    print g, atan(x)
   !
   ! all the quadrants using two arguments
   !
    print g, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad
    print g, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad
    print g, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad
    print g, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad
   !
   ! elemental
   !
    print g, 'elemental:'
    print g, atan(xvals,yvals)*Deg_Per_Rad
    print g, 'elemental:'
   !
   ! when x and y are present, atan(3) is an alias for atan2(2)
   !
    print g, 'For comparison to atan2(3):'
    print g, atan2(xvals,yvals)*Deg_Per_Rad
    print g, 'test1 ',merge('PASSED','FAILED',     &
    & all(atan(xvals,yvals)==atan2(xvals,yvals))), &
    & atan(xvals,yvals)==atan2(xvals,yvals)

end program demo_atan
```
Results:
```text
 > 1.235085437457879
 > .7853981633974483 45.00000000000000
 > 2.356194490192345 135.0000000000000
 > -.7853981633974483 -45.00000000000000
 > -2.356194490192345 -135.0000000000000
 > elemental:
 > 45.0000000000000 45.0000000000000 135.000000000000 135.000000000000
 > -45.0000000000000 -45.0000000000000 -135.000000000000 -135.000000000000
 > For comparison to atan2(3):
 > 45.0000000000000 45.0000000000000 135.000000000000 135.000000000000
 > -45.0000000000000 -45.0000000000000 -135.000000000000 -135.000000000000
 > test1 PASSED T T T T T T T T
```
### **Standard**

FORTRAN 77 for a complex argument; and for two
arguments Fortran 2008

### **See Also**

[**atan2**(3)](#atan2), [**tan**(3)](#tan)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
