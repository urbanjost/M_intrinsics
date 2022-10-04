## ibits

### **Name**

**ibits**(3) - \[BIT:COPY\] Bit extraction

### **Synopsis**
```fortran
    result = ibits(i, pos, len)
```
```fortran
     elemental integer(kind=KIND) function ibits(i,pos,len)

      integer(kind=KIND),intent(in) :: i
      integer(kind=**),intent(in) :: pos
      integer(kind=**),intent(in) :: len
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type
  - The return value is of the same kind as **i**. Otherwise,
    any _integer_ kinds are allowed.
```fortran
```
### **Description**

**ibits** extracts a field of length **len** from **i**, starting from
bit position **pos** and extending left for **len** bits. The result is
right-justified and the remaining bits are zeroed. The value of pos+len
must be less than or equal to the value **bit_size(i)**.

### **Options**

- **i**
  : The type shall be _integer_.

- **pos**
  : The type shall be _integer_. A value of zero refers to the least
  significant bit.

- **len**
  : The type shall be _integer_.

### **Result**

The return value is of type _integer_ and of the same kind as **i**.

### **Standard**

Fortran 95

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibset**(3)](#ibset),
[**iand**(3)](#iand),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_
