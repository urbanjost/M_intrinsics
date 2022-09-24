## aint

### **Name**

**aint**(3) - \[NUMERIC\] Truncate to a whole number

### **Syntax**
```fortran
    result=aint(x [,kind])
```
```fortran
     elemental real(kind=KIND) function iaint(x,kind)

     real(kind=KIND),intent(in)   :: x
     integer,intent(in),optional :: kind
```
where the _kind_ of the result is the same as as **x** unless
**kind** is present.
### **Description**

**aint(x, kind)** truncates its argument to a whole number.

### **Arguments**

- **x**
  : the _real_ value to truncate.

- **kind**
  : an _integer_ initialization expression indicating the
  kind parameter of the result.

### **Returns**

The return value is of type _real_ with the kind type parameter of the
argument if the optional **kind** is absent; otherwise, the kind type
parameter will be given by **kind**.

If the magnitude of **x** is less than one, **aint(x)** returns zero.

If the magnitude is equal to or greater than one then it returns the
largest whole number that does not exceed its magnitude.

The sign is the same as the sign of **x**.

### **Examples**

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64
implicit none
real(kind=sp) :: x4
real(kind=dp) :: x8

   x4 = 4.3210_sp
   x8 = 4.3210_dp
   print *, aint(x4), aint(x8)
   print *
   ! elemental
   print *,aint([ &
    &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0,   &
    &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_aint
```
Results:

```text
     4.00000000       4.0000000000000000

    -2.00000000      -2.00000000      -2.00000000      -2.00000000
    -1.00000000      -1.00000000      -0.00000000       0.00000000
     0.00000000       1.00000000       1.00000000       2.00000000
     2.00000000       2.00000000       2.00000000
```
### **Standard**

FORTRAN 77 and later

### **See Also**

[**anint**(3)](#anint),
[**int**(3)](#int),
[**nint**(3)](#nint),
[**selected_int_kind**(3)](#selected_int_kind),
[**ceiling**(3)](#ceiling),
[**floor**(3)](#floor)

 _fortran-lang intrinsic descriptions_
