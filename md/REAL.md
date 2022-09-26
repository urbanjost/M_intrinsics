## real

### **Name**

**real**(3) - \[TYPE:NUMERIC\] Convert to real type

### **Synopsis**
```fortran
    result = real(x [,kind])
```
```fortran
    elemental real(kind=KIND) function real(x,kind)

     TYPE(kind=KIND),intent(in) :: x
     integer(kind=KINDK),intent(in) :: kind
```
  Where the type of **x** may be _integer_, _real_, or _complex_.

### **Description**

**real(x, kind)** converts its argument **x** to a real type.

For complex values this is similar to the modern complex-part-designator
**%RE** which also designates the real part of a value, accept a
designator can appear on the left-hand side of an assignment as well,
as in **val%re=(3.0,4.0)**.

### **Options**

- **x**
  : Shall be _integer_, _real_, or _complex_ to convert to _real_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

These functions return a _real_ variable or array under the following
rules:

1.  **real**(x) is converted to a default _real_ type if **x** is an _integer_
    or _real_ variable.

2.  **real**(x) is converted to a _real_ type with the magnitude of the _real_
    component of a complex value with kind type parameter of **x**.

3.  **real(x, kind)** is converted to a _real_ type with kind type
    parameter **kind** if **x** is a _complex_, _integer_, or _real_ variable.

### **Examples**

Sample program:
```fortran
program demo_real
use,intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
complex              :: zr = (1.0, 2.0)
doubleprecision      :: xd=huge(3.0d0)
complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)

   print *, real(zr), aimag(zr)
   print *, dble(zd), aimag(zd)

   write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)
end program demo_real
```
Results:

```
 1.00000000       2.00000000
 4.0000000000000000       5.0000000000000000
 1.7976931348623157E+308  1.7976931348623157E+308  1.7976931348623157E+308
```
### **Standard**

FORTRAN 77 and later

### **See Also**

[**dble**(3)](#dble),
[**cmplx**(3)](#cmplx),
[**aimag**(3)](#aimag),
[**int**(3)](#int)

 _fortran-lang intrinsic descriptions_
