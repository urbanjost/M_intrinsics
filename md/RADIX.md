## radix

### **Name**

**radix**(3) - \[NUMERIC MODEL\] Base of a model number

### **Synopsis**
```fortran
   result = radix(x)
```
```fortran
    integer function radix(x)

    TYPE(kind=KIND),intent(in) :: x
```
### **Characteristics**

   where TYPE may be _real_ or _integer_ of any kind KIND.

### **Description**

**radix(x)** returns the base of the model representing the entity **x**.

### **Options**

- **x**
  : Shall be of type _integer_ or _real_

### **Result**

The return value is a scalar of type _integer_ and of the default integer
kind.

### **Examples**

Sample program:

```fortran
program demo_radix
implicit none
   print *, "The radix for the default integer kind is", radix(0)
   print *, "The radix for the default real kind is", radix(0.0)
   print *, "The radix for the doubleprecision real kind is", radix(0.0d0)
end program demo_radix
```

Results:

```text
    The radix for the default integer kind is           2
    The radix for the default real kind is           2
    The radix for the doubleprecision real kind is          2
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
[**range**(3)](#range),
[**rrspacing**(3)](#rrspacing),
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_
