## sinpi

### **Name**

**sinpi**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Circular sine function

### **Synopsis**
```fortran
    result = sinpi(x)
```
```fortran
     elemental real(kind=KIND) function sinpi(x)

      real(kind=KIND) :: x
```
### **Characteristics**

  - **x** may be any _real_
  - **KIND** may be any kind supported by the associated real type of **x**.
  - The returned value will be of the same type and kind as the argument
    **x**.

### **Description**

  **sinpi**(3) computes the circular sine of an angle given the size of the angle
  in half-revolutions.

  **sinpi(X)** is approximately equal to **sin(x\*PI)**.

  The sine of an angle in a right-angled triangle is the ratio of the
  length of the side opposite the given angle divided by the length of
  the hypotenuse.

### **Options**

- **x**
  : The angle in half-revolutions to compute the sine of.

### **Result**

  The return value contains the processor-dependent approximation of
  the sine of **x**.

### **Examples**

Example. **sinpi(1.0)** has the value 0.0 (approximately).

Sample program:

```fortran
program demo_sinpi
implicit none
real    :: x
integer :: i
real,parameter :: PI=acos(-1.0)
   do i=0,8
      x=i*0.25
      write(*,*)'x=',x,' sinpi(x)=',sinpi(x)
   enddo
end program demo_sinpi
```
Results:
```text
 > x=   0.00000000  sinpi(x)=   0.00000000
 > x=  0.250000000  sinpi(x)=   0.707106769
 > x=  0.500000000  sinpi(x)=   1.00000000
 > x=  0.750000000  sinpi(x)=   0.707106769
 > x=   1.00000000  sinpi(x)=  -8.74227766E-08
 > x=   1.25000000  sinpi(x)=  -0.707106888
 > x=   1.50000000  sinpi(x)=  -1.00000000
 > x=   1.75000000  sinpi(x)=  -0.707106531
 > x=   2.00000000  sinpi(x)=   1.74845553E-07
```
### **Standard**

fortran 2023

### **See Also**

 - [**acos**(3)](#acos), [**acosd**(3)](#acosd), [**acospi**(3)](#acospi),
 - [**asin**(3)](#asin), [**asind**(3)](#asind),
 - [**atan2**(3)](#atan2), [**atan2d**(3)](#atan2d), [**atan2pi**(3)](#atan2pi),
 - [**cos**(3)](#cos), [**cosd**(3)](#cosd), [**cospi**(3)](#cospi),
 - [**tan**(3)](#tan), [**tand**(3)](#tand), [**tanpi**(3)](#tanpi),
 - [**acosh**(3)](#acosh),
 - [**acosh**(3)](#acosh),
 - [**asinh**(3)](#asinh),
 - [**asinh**(3)](#asinh),
 - [**atanh**(3)](#atanh)
 - [**atanh**(3)](#atanh),

### **Resources**

- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

 _Fortran intrinsic descriptions (license: MIT) \@urbanjost_
