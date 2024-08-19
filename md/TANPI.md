## tanpi

### **Name**

**tanpi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular tangent function

### **Synopsis**
```fortran
result = tanpi(x)
```
```fortran
 elemental real(kind=KIND) function tanpi(x)

  real(kind=KIND),intent(in) :: x
```
### **Characteristics**

  - the **TYPE** of **x** is _real_ any supported kind
  - The returned value will be of the same type and kind as the argument
    **x**.

### **Description**

   **tanpi**(3) computes the Circular Tangent of **x** in
   half-revolutions.

   The result has a value equal to a processor-dependent approximation
   to the tangent of X, which is regarded as a value in half-revolutions;
   thus, TANPI (X) is approximately equal to tan(X*PI).

### **Options**

- **x**
  : The angle in half-revolutions to compute the tangent of.

### **Result**

  The return value is the tangent of the value **x**.

### **Examples**

Example: TAND(1.0) has the value 0.0 (approximately).

Sample program:
```fortran
program demo_tanpi
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer :: i
real(kind=real64) :: x
   do i=0,8
      x=0.250000000d0*i
      write(*,101)x, tanpi(x), tanpi(x)*180.0d0
   enddo
101 format(g0,t23,g0,t50,g0)
end program demo_tanpi
```
Results:
```text
 > .000000000000000    0.000000000000000          0.000000000000000
 > .2500000000000000   0.9999999999999999       180.0000000000000
 > .5000000000000000   0.1633123935319537E+17     0.2939623083575166E+19
 > .7500000000000000  -1.000000000000000       -180.0000000000000
 > 1.000000000000000  -0.1224646799147353E-15    -0.2204364238465236E-13
 > 1.250000000000000   0.9999999999999997       179.9999999999999
 > 1.500000000000000  5443746451065123.           0.9798743611917221E+18
 > 1.750000000000000  -1.000000000000000       -180.0000000000001
 > 2.000000000000000  -0.2449293598294706E-15    -0.4408728476930472E-13
```
### **Standard**

Fortran 2023

### **See Also**

[**atand**(3)](#atand),
[**atand**(3)](#atand),
[**atan2pi**(3)](#atan2pi),
[**atan2d**(3)](#atan2d)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
