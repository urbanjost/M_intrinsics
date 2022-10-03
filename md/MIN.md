## min

### **Name**

**min**(3) - \[NUMERIC\] Minimum value of an argument list

### **Synopsis**
```fortran
    result = min(a1, a2, a3, ... )
```
```fortran
     elemental TYPE(kind=kind(a1)) function min(a1, a2, a3, ... )

      TYPE(kind=kind(a1),intent(in)   :: a1
      TYPE(kind=kind(a1),intent(in)   :: a2
      TYPE(kind=kind(a1),intent(in)   :: a3
                :
                :
                :
```
### **Characteristics**

Where **TYPE** may be _integer_ or _real_

### **Description**

Returns the argument with the smallest (most negative) value.

### **Options**

- **a1**
  : The type shall be _integer_ or _real_.

- **a2, a3, ...**
  : An expression of the same type and kind as **a1**.

### **Result**

The return value corresponds to the minimum value among the arguments,
and has the same type and kind as the first argument.

### **Examples**

Sample program
```fortran
program demo_min
implicit none
    write(*,*)min(10.0,11.0,30.0,-100.0)
end program demo_min
```
Results:
```
      -100.0000000
```
### **Standard**

FORTRAN 77

### **See Also**

[**maxloc**(3)](#maxloc),
[**minloc**(3)](#minloc),
[**minval**(3)](#minval),
[**max**(3)](#max),

 _fortran-lang intrinsic descriptions_
