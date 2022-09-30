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
     integer(kind=KINDD),intent(in),optional :: dim
     integer(kind=KINDK),intent(in),optional :: kind
```
### **Characteristics**

- **array** Shall be an array, of any type.
- **dim** : (Optional) Shall be a scalar _integer_.
- **kind** An _integer_ initialization expression indicating the kind
  parameter of the result.
- The return value is of type _integer_ and of kind **kind**. If **kind**
  is absent, the return value is of default integer kind.

  If **dim** is absent, the result is an array of the upper bounds of
  **array**.

### **Description**

Returns the upper bounds of an array, or a single upper bound along the
**dim** dimension.

### **Options**

- **array**
  : The array to determine the upper bounds of

- **dim**
  : a specific rank to determine the bounds of

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

Fortran 95 and later, with KIND argument Fortran 2003
and later

### **See Also**

[**lbound**(3)](#lbound),
[**co_ubound**(3)](#co_ubound),
[__co\_lbound__(3)(co_lbound)]

 _fortran-lang intrinsic descriptions_
