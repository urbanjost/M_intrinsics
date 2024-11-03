## acosd

### **Name**

**acosd**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arccosine (inverse cosine) function in degrees

### **Synopsis**
```fortran
    result = acosd(x)
```
```fortran
     elemental real(kind=KIND) function acosd(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

 - **KIND** may be any kind supported by the _real_ type.
 - The returned value will be of the same type and kind as the argument.

### **Description**

   **acosd**(3) computes the arccosine of **x** in degrees (inverse
   of **cosd(x)**). For example, **ACOSD(-1.0)** has the value 180.0
   (approximately).

### **Options**

- **x**
  : The value to compute the arctangent of.
    If the type is _real_, the value must satisfy |**x**| <= 1.

### **Result**

The return value is of the same type and kind as **x**.
The result has a value equal to a processor-dependent approximation to
the arc cosine of X. It is expressed in degrees and lies in the range

    0 <= ACOSD (X) <= 180

### **Examples**

Sample program:

```fortran
program demo_acosd
use, intrinsic :: iso_fortran_env, only : real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x , d2r

   ! basics
    print *,'acosd(-1.0) -->',acosd( -1.0 )
    print *,'acosd( 0.0) -->',acosd( -1.0 )
    print *,'acosd( 1.0) -->',acosd(  0.0 )
    x = 0.866_real64
    print all,'acosd(',x,') is ', acosd(x)
   ! any real kind
    write(*,*) acosd(-1.0_real64)
   ! elemental
    print all,'elemental',acosd([-1.0,-0.5,0.0,0.50,1.0])
   !
end program demo_acosd
```
Results:
```text
 >  acosd(-1.0) -->   180.000000
 >  acosd( 0.0) -->   180.000000
 >  acosd( 1.0) -->   90.0000000
 > acosd( 0.86599999999999999 ) is  30.002910931188026
 >    180.00000000000000
 > elemental 180.000000 120.000000 90.0000000 60.0000000 0.00000000
```
### **Standard**

FORTRAN 2023

### **See Also**
Inverse function: [**cosd**(3)](cosd)

### **Resources**
- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
