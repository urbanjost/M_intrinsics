## atanpi

### **Name**

**atanpi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular Arctangent AKA inverse tangent function

### **Synopsis**
```fortran
    result = atanpi([x) | atanpi(y, x)
```
```fortran
     elemental real(kind=KIND) function atanpi(y,x)

      real(kind=KIND),intent(in) :: x
      real(kind=KIND),intent(in),optional :: y
```
### **Characteristics**

 - **y** and **x** must both be _real_ and of the same KIND
 - **KIND** can be any kind supported by the real type.
 - The returned value is of the same type and kind as **x**.

### **Description**

   **atan**(3) computes the circular arctangent of **x** in
   half-revolutions.

   If **y** appears, the result is the same as the result of
   **atan2pi(y,x)**. If **y** does not appear, the result has a value
   equal to a processor-dependent approximation to the arc tangent of
   **x**; it is expressed in half-revolutions and lies in the range
   **-0.5 <= atanpi(x) <= 0.5**.

   Example. **atanpi(1.0)** has the value 0.25 (approximately).

### **Options**

- **x**
  : The _real_ value to compute the arctangent of.

- **y**
  : is of the same type and kind as **x**. If **x** is zero, **y**
  must not be zero.

### **Result**

   The returned value is of the same type and kind as **x**. If **y**
   is present, the result is identical to **atan2pi(y,x)**. Otherwise,
   it is the arc tangent of **x**, where the result is in half-revolutions
   and lies in the range **-1 \<= atan(x) \<= 1**

### **Examples**

Sample program:

```fortran
program demo_atanpi
use, intrinsic :: iso_fortran_env, only : real32, real64
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x, y
    x=2.866_real64
    print all, atanpi(x)

    print all, atanpi( 2.0d0, 2.0d0),atanpi( 2.0d0, 2.0d0)*180
    print all, atanpi( 2.0d0,-2.0d0),atanpi( 2.0d0,-2.0d0)*180
    print all, atanpi(-2.0d0, 2.0d0),atanpi(-2.0d0, 2.0d0)*180
    print all, atanpi(-2.0d0,-2.0d0),atanpi(-2.0d0,-2.0d0)*180

end program demo_atanpi
```
Results:
```text
 > 0.39313990502447488
 > 0.25000000000000000 45.000000000000000
 > 0.75000000000000000 135.00000000000000
 > -0.25000000000000000 -45.000000000000000
 > -0.75000000000000000 -135.00000000000000
```
### **Standard**

Fortran 2023

### **See Also**

[**atan2d**(3)](#tan2d),
[**tan2d**(3)](#tan2d),
[**atan2pi**(3)](#tan2pi),
[**tan2pi**(3)](#tan2pi)

### **Resources**

- [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

