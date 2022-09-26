## same_type_as

### **Name**

**same_type_as**(3) - \[STATE\] Query dynamic types for equality

### **Synopsis**
```fortran
    result = same_type_as(a, b)
```
```fortran
```
### **Description**

Query dynamic types for equality.

### **Options**

- **a**
  : Shall be an object of extensible declared type or unlimited
  polymorphic.

- **b**
  : Shall be an object of extensible declared type or unlimited
  polymorphic.

### **Result**

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of **a** is the same as the dynamic type of **b**.

### **Standard**

Fortran 2003 and later

### **See Also**

[**extends_type_of**(3)](#extends_type_of)

 _fortran-lang intrinsic descriptions_
