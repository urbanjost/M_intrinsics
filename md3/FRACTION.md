## fraction

### **Name**

**fraction**(3) - \[MODEL:COMPONENTS\] Fractional part of the model representation

### **Synopsis**
```fortran
    result = fraction(x)
```
```fortran
     elemental real(kind=KIND) function fraction(x)

      real(kind=KIND),intent(in) :: fraction
```
### **Characteristics**

  - **x** is of type _real_
  - The result has the same characteristics as the argument.

### **Description**

  **fraction**(3) returns the fractional part of the model representation
  of **x**.

### **Options**

- **x**
  : The value to interrogate

### **Result**

The fractional part of the model representation of **x** is returned;
it is
```fortran
    x * real(radix(x))**(-exponent(x))
```
If **x** has the value zero, the result is zero.

If **x** is an IEEE NaN, the result is that NaN.

If **x** is an IEEE infinity, the result is an IEEE NaN.

### **Examples**

Sample program:

```fortran
program demo_fraction
implicit none
real :: x
   x = 178.1387e-4
   print *, fraction(x), x * real(radix(x))**(-exponent(x))
   x = 10.0
   print *, fraction(x)
   print *, fraction(x) * 2**4
end program demo_fraction
```
Results:
```text
 >   0.570043862      0.570043862
 >   0.625000000
 >    10.0000000
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**huge**(3)](#huge),
[**maxexponent**(3)](#maxexponent),
[**minexponent**(3)](#minexponent),
[**nearest**(3)](#nearest),
[**precision**(3)](#precision),
[**radix**(3)](#radix),
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _Fortran intrinsic descriptions_
