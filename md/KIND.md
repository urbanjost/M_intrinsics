## kind

### **Name**

**kind**(3) - \[KIND INQUIRY\] Kind of an entity

### **Synopsis**
```fortran
    result = kind(x)
```
```fortran
     integer function kind(x)

      type(TYPE,kind=KIND),intent(in) :: x(..)
```
### **Characteristics**

  **TYPE** may _logical_, _integer_, _real_, _complex_ or _character_.

  **x** may be of any kind supported by the type, and may be
  scalar or an array.

### **Description**

   **kind(x)** returns the kind value of the entity **x**.

### **Options**

- **x**
  : Value to query the kind of.

### **Result**

  The return value indicates the kind of the argument **x**.

  Note that kinds are processor-dependent.

### **Examples**

Sample program:
```fortran
program demo_kind
implicit none
integer,parameter :: dc = kind(' ')
integer,parameter :: dl = kind(.true.)

   print *, "The default character kind is ", dc
   print *, "The default logical kind is ", dl

end program demo_kind
```
Results:

```text
    The default character kind is            1
    The default logical kind is            4
```
### **Standard**

Fortran 95

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
