## iand

### **Name**

**iand**(3) - \[BIT:LOGICAL\] Bitwise logical and

### **Synopsis**
```fortran
    result = iand(i, j)
```
```fortran
     elemental integer(kind=KIND) function iand(i,j)

      integer(kind=KIND),intent(in) :: i
      integer(kind=KIND),intent(in) :: j
```
### **Description**

Bitwise logical **and**.

### **Options**

- **i**
  : The type shall be _integer_.

- **j**
  : The type shall be _integer_, of the same kind as **i**.

### **Result**

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### **Examples**

Sample program:

```fortran
program demo_iand
implicit none
integer :: a, b
      data a / z'f' /, b / z'3' /
      write (*,*) iand(a, b)
end program demo_iand
```

Results:

```text
              3
```

### **Standard**

Fortran 95 and later

### **See Also**

[**ieor**(3)](#ieor),
[**ibclr**(3)](#ibclr),
[**not**(3)](#not),
[**btest**(3)](#btest),
[**ibclr**(3)](#ibclr),
[**ibits**(3)](#ibits),
[**ibset**(3)](#ibset),
[**ior**(3)](#ior),
[**ieor**(3)](#ieor),
[**mvbits**(3)](#mvbits)

 _fortran-lang intrinsic descriptions_
