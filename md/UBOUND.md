## ubound

### **Name**

**ubound**(3) - \[ARRAY INQUIRY\] Upper dimension bounds of an array

### **Synopsis**
```fortran
    result = ubound(array [,dim] [,kind] )
```
```fortran
     elemental TYPE(kind=KIND) function ubound(array,dim,kind)

      TYPE(kind=KIND),intent(in)  :: array
      integer(kind=**),intent(in),optional :: dim
      integer(kind=**),intent(in),optional :: kind
```
### **Characteristics**

- a kind designated as ** may be any supported kind value for the type
- **array** shall be an array, of any type.
- **dim** shall be a scalar _integer_.
- **kind** an _integer_ initialization expression indicating the kind
  parameter of the result.
- The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default integer kind.

### **Description**

**ubound**(3) returns the upper bounds of an array, or a single upper
bound along the **dim** dimension.

### **Options**

- **array**
  : The array to determine the upper bounds of

- **dim**
  : a specific rank to determine the bounds of
  If **dim** is absent, the result is an array of the upper bounds of
  **array**.

- **kind**
  : indicates the kind parameter of the result. If absent, an _integer_
  of the default kind is returned.

### **Result**

The return value is of type _integer_ and of kind **kind**. If **kind**
is absent, the return value is of default integer kind.

If **dim** is absent, the result is an array of the upper bounds of
**array**.

If **dim** is present, the result is a scalar corresponding to the upper
bound of the array along that dimension.

If **array** is an expression rather than a whole array or array
structure component, or if it has a zero extent along the relevant
dimension, the upper bound is taken to be the number of elements along
the relevant dimension.

### **Examples**

Note this function should not be used on assumed-size arrays or in any
function without an explicit interface. Errors can occur if there is no
interface defined.

Sample program

```fortran
! program demo_ubound
module m2_bounds
implicit none

contains

subroutine msub(arr)
!!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
integer,intent(in) :: arr(:)
   write(*,*)'MSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine msub

end module m2_bounds

use m2_bounds, only : msub
implicit none
interface
   subroutine esub(arr)
   integer,intent(in) :: arr(:)
   end subroutine esub
end interface
integer :: arr(-10:10)
   write(*,*)'MAIN: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
   call csub()
   call msub(arr)
   call esub(arr)
contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub

end

subroutine esub(arr)
implicit none
integer,intent(in) :: arr(:)
   ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
   ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
   write(*,*)'ESUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine esub
!end program demo_ubound
```

Results:

```text
  MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
  CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
  MSUB: LOWER=           1 UPPER=          21 SIZE=          21
  ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```

### **Standard**

Fortran 95 , with KIND argument Fortran 2003


### **See Also**


#### Array inquiry:

- [**size**(3)](#size) -  Determine the size of an array
- [**rank**(3)](#rank) -  Rank of a data object
- [**shape**(3)](#shape) -  Determine the shape of an array
- [**ubound**(3)](#ubound) -  Upper dimension bounds of an array
- [**lbound**(3)](#lbound) -  Lower dimension bounds of an array

[**co_ubound**(3)](#co_ubound),
[__co\_lbound__(3)(co_lbound)]
 
#### State Inquiry:

- [**allocated**(3)](#allocated) -  Status of an allocatable entity
- [**is_contiguous**(3)](#is_contigious) -  Test if object is contiguous

#### Kind Inquiry:

- [**kind**(3)](#kind) - Kind of an entity

#### Bit Inquiry: 

- [**storage_size**(3)](#storage_size) - Storage size in bits
- [**bit_size**(3)](#bit_size) -  Bit size inquiry function
- [**btest**(3)](#btest) - Tests a bit of an _integer_ value.
- [**lbound**(3)](#lbound),

 _fortran-lang intrinsic descriptions (license: MIT) \@urbanjost_
