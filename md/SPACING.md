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
### **Characteristics**

  The result is of the same type as the input argument **x**.

### **Description**

  **spacing**(3) determines the distance between the argument **x**
  and the nearest adjacent number of the same type.

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

   write(*,*) spacing(1.0_sgl)
   write(*,*) nearest(1.0_sgl,+1.0),nearest(1.0_sgl,+1.0)-1.0

   write(*,*) spacing(1.0_dbl)
end program demo_spacing
```
Results:

Typical values ...

```text
     1.1920929E-07
      1.000000      1.1920929E-07
     0.9999999     -5.9604645E-08
     2.220446049250313E-016
```
### **Standard**

Fortran 95

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
