## tand

### **Name**

**tand**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Degree Tangent function

### **Synopsis**
```fortran
result = tand(x)
```
```fortran
 elemental real(kind=KIND) function tand(x)

  real(kind=KIND),intent(in) :: x
```
### **Characteristics**

  - the **TYPE** of **x** is _real_ of any supported kind
  - The returned value will be of the same type and kind as the argument
    **x**.

### **Description**

**tand**(3) computes the degree tangent of **x**.

### **Options**

- **x**
  : The angle in degrees to compute the tangent of.

### **Result**

  The return value is a processor-dependent approximation to the tangent
  of the value **x** where **x** is regarded as a value in degrees.

### **Examples**

tand(180.0) has the value 0.0 (approximately).

Sample program:
```fortran
program demo_tand
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 0.5_real64
     write(*,*)x, tand(x)
end program demo_tand
```
### **Standard**

Fortran 2023

### **See Also**

[**atand**(3)](#atand),
[**atan**(3)](#atan),
[**atan2d**(3)](#atan2d),
[**atan2**(3)](#atan2),
[**cosd**(3)](#cosd),
[**sind**(3)](#sind)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_

