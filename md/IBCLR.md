#e# ibclr

### **Name**

**ibclr**(3) - \[BIT:SET\] Clear bit

### **Synopsis**
```fortran
    result = ibclr(i, pos)
```
```fortran
     elemental integer(kind=KIND) function ibclr(i,pos)

      integer(kind=KIND),intent(in) :: i
      integer(kind=**),intent(in) :: pos
```
### **Characteristics**
  - a kind designated as ** may be any supported kind value for the type

  - The return value is of the same kind as **i**. Otherwise,
    any _integer_ kinds are allowed.

### **Description**

**ibclr** returns the value of **i** with the bit at position **pos** set to zero.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit. **pos** is an **intent(in)** scalar or array of type
  _integer_. The value of **pos** must be within the range zero to
  **(bit_size(i)-1**).

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_
