## ibset

### **Name**

**ibset**(3) - \[BIT:SET\] Set bit

### **Synopsis**
```fortran
    result = ibset(i, pos)
```
```fortran
     elemental integer(kind=KINDI) function ibset(i,pos)

     integer(kind=KINDI),intent(in) :: i
     integer(kind=KINDPOS),intent(in) :: pos
```
### **Characteristics**

  The return value is of the same kind as **i**. Otherwise,
  any _integer_ kinds are allowed.

### **Description**

**ibset** returns the value of **i** with the bit at position **pos** set to one.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit. pos is an **intent(in)** scalar or array of type
  _integer_. The value of pos must be within the range zero to
  **(bit_size(i)-1**).

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95 and later

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_
