## pack

### **Name**

**pack**(3) - \[ARRAY:CONSTRUCTION\] Pack an array into an array of rank one

### **Synopsis**
```fortran
    result = pack( array, mask [,vector] )
```
```fortran
     TYPE(kind=KIND) function pack(array,mask,vector)

      TYPE(kind=KIND),option(in) :: array(..)
      logical  :: mask(*)
      TYPE(kind=KIND),option(in),optional :: vector(*)
```
### **Characteristics**

  - **array**, **vector** and the returned value are all of the same kind and type.
  - **mask** may be a scalar as well an an array.

### **Description**

  **pack**(3) stores the elements of ARRAY in an array of rank one.

  The beginning of the resulting array is made up of elements whose
  **mask** equals _\.true._. Afterwards, positions are filled with elements
  taken from **vector**.

### **Options**

- **array**
  : The data from this array is used to fill the resulting vector

- **mask**
  : the _logical_ mask must be the same size as **array** or,
  alternatively, it may be a _logical_ scalar.

- **vector**
  : (Optional) shall be an array of the same type as **array** and of rank
  one. If present, the number of elements in **vector** shall be equal to
  or greater than the number of true elements in **mask**. If **mask** is
  scalar, the number of elements in **vector** shall be equal to or
  greater than the number of elements in **array**.

### **Result**

The result is an array of rank one and the same type as that of **array**.
If **vector** is present, the result size is that of **vector**, the number of
_\.true._ values in **mask** otherwise.

### **Examples**

Sample program:

```fortran
program demo_pack
implicit none
integer, allocatable :: m(:)
character(len=10) :: c(4)

 ! gathering nonzero elements from an array:
   m = [ 1, 0, 0, 0, 5, 0 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)

 ! Gathering nonzero elements from an array and appending elements
 ! from VECTOR till the size of the mask array (or array size if the
 ! mask is scalar):
   m = [ 1, 0, 0, 2 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])

 ! select strings whose second character is "a"
   c = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
   write(*, fmt="(*(g0, ' '))") pack(c, c(:)(2:2) == 'a' )

end program demo_pack
```
Results:
```text
   1 5
   1 2 3 4
   bat        cat
```
### **Standard**

Fortran 95

### **See Also**

[**merge**(3)](#merge),
[**spread**(3)](#spread),
[**unpack**(3)](#unpack)

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
