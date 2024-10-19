## atand

### **Name**

**atand**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arc tangent AKA inverse
tangent function in degrees

### **Synopsis**
```fortran
    result = atand(x) | atand(y, x)
```
```fortran
     elemental real(kind=KIND) function atand(y,x)

      real(kind=KIND),intent(in) :: x
      real(kind=**),intent(in),optional :: y
```
### **Characteristics**

 - If **y** is present **x** and **y** must both be of the same
   kind.
 - **KIND** can be any kind supported by the associated type.
 - The returned value is real of the same kind as **x**.

### **Description**

**atand**(3) calculates the Arc Tangent function in degrees.

### **Options**

- **x**
  : The _real_ value to compute the arctangent of.

- **y**
  : is real of the same kind as **x**. If **x** is zero, **y**
  must not be zero.

### **Result**

The returned value is a _real_ type of the same kind as **x** that
approximates the arc tangent of **x** expressed in degrees. If **y**
is present, the result is identical to **atan2d(y,x)**. The result lies
in the range **-90 \<= atand(x) \<= 90** .

### **Examples**

atand(1.0) has the value 45.0 (approximately).

Sample program:

```fortran
program demo_atand
use, intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64),parameter :: &
 Deg_Per_Rad = 57.2957795130823208767981548_real64
real(kind=real64) :: x
    x=2.866_real64
    print all, atand(x)

    print all, atand( 2.0d0, 2.0d0),atand( 2.0d0, 2.0d0)/Deg_Per_Rad
    print all, atand( 2.0d0,-2.0d0),atand( 2.0d0,-2.0d0)/Deg_Per_Rad
    print all, atand(-2.0d0, 2.0d0),atand(-2.0d0, 2.0d0)/Deg_Per_Rad
    print all, atand(-2.0d0,-2.0d0),atand(-2.0d0,-2.0d0)/Deg_Per_Rad

end program demo_atand
```
Results:
```text
 > 70.765182904405478
 > 45.000000000000000 0.78539816339744828
 > 135.00000000000000 2.3561944901923448
 > -45.000000000000000 -0.78539816339744828
 > -135.00000000000000 -2.3561944901923448
```
### **Standard**

Fortran 2023

### **See Also**

[**atan2d**(3)](#atand2), [**tand**(3)](#tand),
[**atan2**(3)](#atan2), [**tan**(3)](#tan),
[**atan2pi**(3)](#atan2pi), [**tanpi**(3)](#tanpi)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
