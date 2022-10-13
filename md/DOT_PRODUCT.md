## dot_product

### **Name**

**dot_product**(3) - \[TRANSFORMATIONAL\] Dot product function

### **Synopsis**
```fortran
    result = dot_product(vector_a, vector_b)
```
```fortran
     TYPE(kind=KIND) function dot_product(vector_a, vector_b)

      TYPE(kind=KIND),intent(in) :: vector_a(:)
      TYPE(kind=KIND),intent(in) :: vector_b(:)
```
### **Characteristics**

The two vectors may be either numeric or logical and must be arrays
of rank one and of equal size.

### **Description**

**dot_product**(3) computes the dot product
multiplication of two vectors **vector_a** and **vector_b**.

If the vectors are _integer_ or _real_, the result is
```fortran
     sum(vector_a*vector_b)
```
If the vectors are _complex_, the result is
```fortran
     sum(conjg(vector_a)*vector_b)**
```
If the vectors are _logical_, the result is
```fortran
     any(vector_a .and. vector_b)
```
### **Options**

- **vector_a**
  : The type shall be numeric or _logical_, rank 1.

- **vector_b**
  : The type shall be numeric if vector_a is of numeric type or _logical_
  if vector_a is of type _logical_. vector_b shall be a rank-one
  array.

### **Result**

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is _\.true._ or _\.false._.

### **Examples**

Sample program:

```fortran
program demo_dot_prod
implicit none
    integer, dimension(3) :: a, b
    a = [ 1, 2, 3 ]
    b = [ 4, 5, 6 ]
    print '(3i3)', a
    print *
    print '(3i3)', b
    print *
    print *, dot_product(a,b)
end program demo_dot_prod
```
Results:
```text
  >  1  2  3
  >
  >  4  5  6
  >
  >           32
```
### **Standard**

Fortran 95

### **See Also**

[****(3)](#)

 _fortran-lang intrinsic descriptions_
