## acospi

### **Name**

**acospi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular Arccosine (inverse
circular cosine) function

### **Synopsis**
```fortran
    result = acospi(x)
```
```fortran
     elemental real(kind=KIND) function acospi(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **KIND** may be any _real_ kind
 - The returned value will be of the same type and kind as the argument.

### **Description**

**acospi**(3) computes the circular arccosine of **x** (inverse of
**cospi(x)**). The result is expressed in half-revolutions (ie. PI's)
and lies in the range
```fortran
    0 <= ACOSPI (X) <= 1.
```

### **Options**

- **x**
  : The value to compute the circular arctangent of.
  : The value must satisfy |**x**| <= 1.

### **Result**

The result has a value equal to a processor-dependent approximation to
the arc cosine of X.

The return value is of the same type and kind as **x**.

It is expressed in half-revolutions and lies in the range 0 <= ACOSPI (X) <= 1.

### **Examples**

Sample program:

```fortran
program demo_acospi
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x , d2r
real(kind=real64),parameter :: &
& PI = 3.14159265358979323846264338327950288419716939937510_real64

   ! basics
    x = PI/4.0_real64
    print all,'acospi(',x,') is ', acospi(x)

   ! acospi(-1) should be PI
    write(*,*) acospi(-1.0_real64)
    d2r=acospi(-1.0_real64)/180.0_real64
    print all,'90 degrees is ', d2r*90.0_real64, ' radians'
   ! elemental
    print all,'elemental',acospi([-1.0,-0.5,0.0,0.50,1.0])
   ! 
    print *,'-1.0',acospi( -1.0 )
    print *,' 0.0',acospi(  0.0 )
    print *,' 1.0',acospi(  1.0 )

end program demo_acospi
```
Results:
```text
```
### **Standard**

Fortran 2023 

### **See Also**
 - arc cosine in radians: [**acos**(3)](cos)
 - arc cosine in degrees: [**acosd**(3)](cos)
 - Inverse function: [**cos**(3)](cos)

### **Resources**
- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
