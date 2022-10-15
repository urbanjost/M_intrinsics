## parity

### **Name**

**parity**(3) - \[TRANSFORMATIONAL\] Reduction with exclusive **OR**()

### **Synopsis**
```fortran
    result = parity( mask [,dim] )
```
```fortran
     logical(kind=KIND) function parity(mask, dim)

      type(logical(kind=KIND)),intent(in)        :: mask(..)
      type(integer(kind=**)),intent(in),optional :: dim
```
### **Characteristics**

  - a kind designated as ** may be any supported kind for the type

### **Description**

**parity**(3) calculates the parity (i.e. the reduction using .xor.) of
**mask** along dimension **dim**.

### **Options**

  - **mask**
    : Shall be an array of type _logical_.

  - **dim**
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from _1 to n_, where _n_ equals the rank of **mask**.

### **Result**

  The result is of the same type as **mask**.

  If **dim** is absent, a scalar with the parity of all elements in **mask**
  is returned: _.true._ if an odd number of elements are _.true._
  and _.false._ otherwise.

  When **dim** is specified the returned shape is similar to that of
  **mask** with dimension **dim** dropped.

### **Examples**

Sample program:
```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x)
end program demo_parity
```
Results:
```text
    T
```
### **Standard**

Fortran 2008

### **See also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_
