## logical

### **Name**

**logical**(3) - \[TYPE:LOGICAL\] Converts one kind of _logical_ variable to another

### **Synopsis**
```fortran
    result = logical(l [,kind])
```
```fortran
     elemental logical(kind=KIND) function logical(l,kind)

      logical(kind=KIND),intent(in) :: l
      integer(kind=**),intent(in),optional :: kind
```
### **Characteristics**

  - a kind designated as ** may be any supported kind value for the type

### **Description**

  **logical**(3) converts one kind of _logical_ variable to another.

### **Options**

- **l**
  : The type shall be _logical_.

- **kind**
  : An _integer_ initialization expression indicating the kind parameter
  of the result. If not present, the default kind is returned.

### **Result**

The return value is a _logical_ value equal to **l**, with a kind
corresponding to **kind**, or of the default logical kind if **kind**
is not given.

### **Examples**

Sample program:
```fortran
program demo_logical
! Access array containing the kind type parameter values supported by this
! compiler for entities of logical type
use iso_fortran_env, only : logical_kinds

   ! list kind values supported on this platform, which generally vary
   ! in storage size
   do i =1, size(logical_kinds)
      write(*,*)logical_kinds(i)
   enddo

end program demo_logical
```
Results:
```text
              1
              2
              4
              8
             16
```
### **Standard**

Fortran 95 , related ISO_FORTRAN_ENV module - fortran 2009

### **See Also**

[**int**(3)](#int),
[**real**(3)](#real),
[**cmplx**(3)](#cmplx)

 _fortran-lang intrinsic descriptions_
