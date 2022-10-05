## ieor

### **Name**

**ieor**(3) - \[BIT:LOGICAL\] Bitwise logical exclusive or

### **Synopsis**
```fortran
    result = ieor(i, j)
```
```fortran
     elemental integer(kind=KIND) function ieor(i,j)

      integer(kind=**),intent(in) :: i
      integer(kind=**),intent(in) :: j
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

  - any _integer_ kinds are allowed for **i** and **j**.

  - The return value is of the same kind as the larger kind of **i**
    and **j**.

### **Description**

**ieor**(3) returns the bitwise Boolean exclusive-**or** of **i** and **j**.

### **Options**

- **i**
  : The type shall be _integer_.

- **j**
  : The type shall be _integer_, of the same kind as **i**.

### **Result**

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_
