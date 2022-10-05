## maxexponent

### **Name**

**maxexponent**(3) - \[NUMERIC MODEL\] Maximum exponent of a real kind

### **Synopsis**
```fortran
    result = maxexponent(x)
```
```fortran
     elemental integer function maxexponent(x)

      real(kind=KIND),intent(in)   :: x
```
### **Characteristics**

where KIND is any _real_ kind.

### **Description**

**maxexponent**(3) returns the maximum exponent in the model of the type
of **x**.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The return value is of type _integer_ and of the default integer kind.

### **Examples**

Sample program:
```fortran
program demo_maxexponent
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x
real(kind=dp) :: y

   print *, minexponent(x), maxexponent(x)
   print *, minexponent(y), maxexponent(y)
end program demo_maxexponent
```
Results:
```text
           -125         128
          -1021        1024
```
### **Standard**

Fortran 95

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
[**huge**(3)](#huge),
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

 _fortran-lang intrinsic descriptions_
