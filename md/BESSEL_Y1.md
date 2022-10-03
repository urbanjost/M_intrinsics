## bessel_y1

### **Name**

**bessel_y1**(3) - \[MATHEMATICS\] Bessel function of the second kind of order 1

### **Synopsis**
```fortran
    result = bessel_y1(x)
```
```fortran
     elemental real(kind=KIND) function bessel_y1(x)

      real(kind=KIND),intent(in) :: x
```
### **Characteristics**

where KIND may be any supported _real_ KIND.

### **Description**

**bessel_y1(x)** computes the Bessel function of the second
kind of order 1 of **x**.

### **Options**

- **x**
  : The type shall be _real_.

### **Result**

The return value is _real_. It has the same kind as **x**.

### **Examples**

Sample program:

```fortran
program demo_bessel_y1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
  real(kind=real64) :: x = 1.0_real64
  write(*,*)x, bessel_y1(x)
end program demo_bessel_y1
```

### **Standard**

Fortran 2008

### **See Also**

[**bessel_j0**(3)](#bessel_j0),
[**bessel_j1**(3)](#bessel_j1),
[**bessel_jn**(3)](#bessel_jn),
[**bessel_y0**(3)](#bessel_y0),
[**bessel_yn**(3)](#bessel_yn)

 _fortran-lang intrinsic descriptions_
