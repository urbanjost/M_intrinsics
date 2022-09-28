## iparity

### **Name**

**iparity**(3) - \[BIT:LOGICAL\] Bitwise exclusive or of array elements

### **Synopsis**
```fortran
    result = iparity(array [,mask])
```
```fortran
     integer(kind=KIND) function iparity(array, mask )

     integer(kind=KIND),intent(in) :: array(..)
     logical(kind=KIND),intent(in),optional :: mask(..)
```
   **array** must be an array. **mask** may be either an array of the
   same shape as **array** or a scalar.

or
```fortran
    result = iparity(array, dim [,mask] )
```
```fortran
     integer(kind=KIND) function iparity( array ,dim ,mask )

     integer(kind=KIND),intent(in)          :: array(..)
     logical(kind=KIND),intent(in)          :: dim
     logical(kind=KIND),intent(in),optional :: mask(..)
```
### **Description**

Reduces with bitwise _xor_ (exclusive _or_) the elements of **array** along
dimension **dim** if the corresponding element in **mask** is _.true._.

### **Options**

- **array**
  : Shall be an array of type _integer_

- **dim**
  : (Optional) shall be a scalar of type _integer_ with a value in the
  range from **"1" to "n"**, where **"n"** equals the rank of **array**.

- **mask**
  : (Optional) shall be of type _logical_ and either be a scalar or an
  array of the same shape as **array**.

### **Result**

The result is of the same type as **array**.

If **dim** is absent, a scalar with the bitwise _xor_ of all elements in **array**
is returned. Otherwise, an array of rank **n-1**, where **n** equals the
rank of **array**, and a shape similar to that of **array** with dimension **dim**
dropped is returned.

### **Examples**

Sample program:
```fortran
program demo_iparity
implicit none
integer, dimension(2) :: a
  a(1) = int(b'00100100')
  a(2) = int(b'01101010')
  print '(b8.8)', iparity(a)
end program demo_iparity
```
Results:
```
   01001110
```
### **Standard**

Fortran 2008 and later

### **See Also**

[**iany**(3)](#iany),
[**iall**(3)](#iall),
[**ieor**(3)](#ieor),
[**parity**(3)](#parity)

 _fortran-lang intrinsic descriptions_
