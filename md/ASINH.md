## asinh

### **Name**

**asinh**(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function

### **Synopsis**
```fortran
    result = asinh(x)
```
```fortran
     elemental TYPE(kind=KIND) function asinh(x)

     TYPE(kind=KIND) :: x
```
### **Characteristics**

where **TYPE** may be _real_ or _complex_ and **KIND** may be any kind supported
by the associated type. The returned value will be of the same type and kind as
the argument.

### **Description**

**asinh(x)** computes the inverse hyperbolic sine of **x**.

### **Options**

- **x**
  : The value to compute the inverse hyperbolic sine of

### **Result**

The return value is of the same type and kind as **x**. If **x** is
_complex_, the imaginary part of the result is in radians and lies
between **-PI/2 \<= aimag(asinh(x)) \<= PI/2**.

### **Examples**

Sample program:
```fortran
program demo_asinh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]

    write (*,*) asinh(x)

end program demo_asinh
```
Results:
```text
  -0.88137358701954305  0.0000000000000000  0.88137358701954305
```
### **Standard**

Fortran 2008 and later

### **See Also**

Inverse function: [**sinh**(3)](#sinh)

### **Resources**

- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

 _fortran-lang intrinsic descriptions_
