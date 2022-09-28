## spacing

### **Name**

**spacing**(3) - \[MODEL_COMPONENTS\] Smallest distance between two numbers of a given type

### **Synopsis**
```fortran
    result = spacing(x)
```
```fortran
     elemental real(kind=KIND) function spacing(x)

      real(kind=KIND), intent(in) :: x
```
  The result is of the same type as the input argument **x**.

### **Description**

Determines the distance between the argument **x** and the nearest adjacent
number of the same type.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The result is of the same type as the input argument **x**.

### **Examples**

Sample program:

```fortran
program demo_spacing
implicit none
integer, parameter :: sgl = selected_real_kind(p=6, r=37)
integer, parameter :: dbl = selected_real_kind(p=13, r=200)

   write(*,*) spacing(1.0_sgl)      ! "1.1920929e-07"          on i686
   write(*,*) spacing(1.0_dbl)      ! "2.220446049250313e-016" on i686
end program demo_spacing
```

Results:

```text
      1.19209290E-07
      2.2204460492503131E-016
```

### **Standard**

Fortran 95 and later

### **See Also**

[**digits**(3)](#digits),
[**epsilon**(3)](#epsilon),
[**exponent**(3)](#exponent),
[**fraction**(3)](#fraction),
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
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_
