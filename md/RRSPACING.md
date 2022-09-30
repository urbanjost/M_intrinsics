## rrspacing

### **Name**

**rrspacing**(3) - \[MODEL_COMPONENTS\] Reciprocal of the relative spacing

### **Synopsis**
```fortran
    result = rrspacing(x)
```
```fortran
     elemental real(kind=KIND) function rrspacing(x)
     real(kind=KIND),intent(in) :: x
```
### **Characteristics**

The return value is of the same type and kind as **x**.

### **Description**

**rrspacing(x)** returns the reciprocal of the relative spacing of model
numbers near **x**.

### **Options**

- **x**
  : Shall be of type _real_.

### **Result**

The return value is of the same type and kind as **x**. The value returned
is equal to **abs(fraction(x)) \* float(radix(x))\*\*digits(x)**.

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
[**scale**(3)](#scale),
[**set_exponent**(3)](#set_exponent),
[**spacing**(3)](#spacing),
[**tiny**(3)](#tiny)

 _fortran-lang intrinsic descriptions_
