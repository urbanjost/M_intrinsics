## lcobound

### **Name**

**lcobound**(3) - \[COLLECTIVE\] Lower codimension bounds of an array

### **Synopsis**
```fortran
     result = lcobound( coarray [,dim] [,kind] )
```
```fortran
```
### **Characteristics**

### **Description**

**lcobound**(3) returns the lower bounds of a coarray, or a single
lower cobound along the **dim** codimension.

### **Options**

- **array**
  : Shall be an coarray, of any type.

- **dim**
  : (Optional) Shall be a scalar _integer_.

- **kind**
  : (Optional) An _integer_ initialization expression indicating the kind
  parameter of the result.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind** is absent,
the return value is of default integer kind. If **dim** is absent, the
result is an array of the lower cobounds of **coarray**. If **dim** is present,
the result is a scalar corresponding to the lower cobound of the array
along that codimension.

### **Standard**

Fortran 2008

### **See Also**

[**ucobound**(3)](#ucobound),
[**lbound**(3)](#lbound)

 _Fortran intrinsic descriptions_
